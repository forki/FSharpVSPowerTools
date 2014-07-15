﻿namespace FSharpVSPowerTools.Refactoring
 
open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharpVSPowerTools.IdentifierUtils

open FSharp.ViewModule
open FSharp.ViewModule.Progress
open FSharp.ViewModule.Validation
open FSharp.Control
open FSharpVSPowerTools.Refactoring.RenameSuggestions

type RenameDialog = FsXaml.XAML<"RenameDialog.xaml">

// The ViewModel for our dialog
type RenameDialogViewModel(originalName: string, symbol: Symbol, initializationWorkflow : Async<(ParseAndCheckResults * SymbolDeclarationLocation * FSharpSymbol) option>, renameWorkflow : (ParseAndCheckResults -> SymbolDeclarationLocation -> string-> (OperationState -> unit) -> Async<unit>), cts : System.Threading.CancellationTokenSource) as self =
    inherit ViewModelBase()

    let originalName = originalName.Replace(DoubleBackTickDelimiter,"")

    // This will hold the actual rename workflow arguments after the initialization async workflow completes    
    let mutable workflowArguments : (FSharpSymbol * SymbolDeclarationLocation * ParseAndCheckResults) option = None

    // Grab our synchronization context for use in async workflows as necessary
    let syncCtx = System.Threading.SynchronizationContext.Current

    // Custom validation for the name property
    let validateSymbols newName =
        let check validationCheck error = if validationCheck then None else Some error

        debug "[Rename Refactoring] Check the following name: %s" newName
        match workflowArguments with
        | None -> Some Resource.renameErrorMessage
        | Some(fssym, _, _) ->
            match symbol.Kind, fssym with
            | _, :? FSharpUnionCase ->                
                check (isUnionCaseIdent newName) Resource.validatingUnionCase 
            | _, :? FSharpActivePatternCase ->
                    // Different from union cases, active patterns don't accept double-backtick identifiers
                    check (isFixableIdentifier newName && not (String.IsNullOrEmpty newName) && Char.IsUpper(newName.[0])) Resource.validatingActivePattern
            | Operator, _ -> 
                check (isOperator newName) Resource.validatingOperator
            | GenericTypeParameter, _ -> 
                check (isGenericTypeParameter newName) Resource.validatingGenericTypeParameter
            | StaticallyResolvedTypeParameter, _ ->
                check (isStaticallyResolvedTypeParameter newName) Resource.validatingStaticallyResolvedTypeParameter
            | (Ident | Other), _ ->
                if SourceCodeClassifier.getIdentifierCategory fssym <> SourceCodeClassifier.Category.Other then
                    check (isTypeNameIdent newName) Resource.validatingTypeName
                else
                    check (isFixableIdentifier newName) Resource.validatingIdentifier

    // Complete validation chain for the name property
    let validateName = 
        validate "Name" 
        >> notNullOrWhitespace 
        >> fixErrorsWithMessage Resource.validatingEmptyName
        >> custom validateSymbols
        >> result

    // Backing fields for all view-bound values
    let name = self.Factory.Backing(<@@ self.Name @@>, originalName, validateName)
    let mutable symbolLocation = ""
    let fullName = self.Factory.Backing(<@@ self.FullName @@>, String.Empty)

    // RenameComplete is used to close our dialog from the View automatically - should be set when we want to "complete" this operation
    let renameComplete = self.Factory.Backing(<@@ self.RenameComplete @@>, false)
    // Initialized allows us to track when the initialization async workflow completes
    let initialized = self.Factory.Backing(<@@ self.Initialized @@>, false)    
    // Create a progress manager to track the progress status
    let progress = ProgressManager()    

    // This provides us a simple way to report progress to both the status bar and our dialog
    let report = Some(updateProgress(progress))

    // We wrap up the actual async workflow in order to close us when renameComplete as well as mark us executing while we run
    let wrappedWorkflow _ newName =
        async {            
            use waitCursor = Cursor.wait()  
            reportProgress report (Reporting("Performing Rename..."))
            // If the user just accepts with the original name, just make this a no-op instead of an error
            if not(newName = originalName) then
                match workflowArguments with
                | Some(_, location, results) ->
                    do! renameWorkflow results location newName report.Value
                | _ -> ()
            renameComplete.Value <- true
            waitCursor.Restore()
        }

    // The async command which is executed when the user clicks OK
    let execute = self.Factory.CommandAsyncParamChecked(wrappedWorkflow, (fun _ -> self.IsValid && self.Initialized), [ <@@ self.IsValid @@> ; <@@ self.Initialized @@> ], cts.Token)
    
    // Cancelling the operation should cancel all async workflows and close us    
    let cancelCommand = self.Factory.CommandSync(fun _ -> cts.Cancel())

    // Generate the new name and show it on the textbox
    let updateFullName newName = 
        let encapsulated = 
            if validateName newName = [] then
                encapsulateIdentifier symbol.Kind newName
            else 
                newName
        if String.IsNullOrEmpty symbolLocation then self.FullName <- encapsulated
        elif String.IsNullOrEmpty encapsulated then self.FullName <- symbolLocation
        else self.FullName <- symbolLocation + "." + encapsulated

    do  
        // Make us close if we're canceled
        cts.Token.Register(fun _ -> renameComplete.Value <- true) |> ignore

        // Force initialization to re-evaluate Name's validation by making it a dependency      
        self.DependencyTracker.AddPropertyDependency(<@@ self.Name @@>, <@@ self.Initialized @@>)
        
        // Perform our initialization routine while reporting progress/etc as necessary
        Async.StartImmediateSafe(
            async {              
                use waitCursor = Cursor.wait()   
                reportProgress report (Reporting("Initializing..."))
                let! b = initializationWorkflow 
                match b with
                | Some(results, location, symbol') ->                            
                    do! Async.SwitchToContext syncCtx
                    workflowArguments <- Some(symbol', location, results)                    
                    reportProgress report Idle
                    symbolLocation <- 
                        let fullName = symbol'.FullName
                        let displayName = symbol'.DisplayName
                        if fullName.EndsWith displayName then
                            let locationLength = max 0 (fullName.Length - (displayName.Length + 1))
                            fullName.Remove locationLength
                        else fullName
                    updateFullName originalName

                    // Start our suggestions
                    let kind = 
                        match symbol.Kind with
                        | SymbolKind.Ident ->
                                match SourceCodeClassifier.getIdentifierCategory symbol' with
                                | SourceCodeClassifier.Category.Function -> Kind.Variable
                                | SourceCodeClassifier.Category.ReferenceType -> Kind.Type
                                | SourceCodeClassifier.Category.ValueType -> Kind.Type
                                | SourceCodeClassifier.Category.Module -> Kind.Type
                                | _ -> Kind.Variable
                        | _ -> Kind.Type
                                        
                    // Mark initialized and bind collection now
                    initialized.Value <- true                    
                    CollectionHelpers.bindObservableToCollectionOnContext syncCtx self.Suggestions (suggest kind originalName) 
                    |> ignore
                | None -> 
                    cts.Cancel()
                waitCursor.Restore()
            }, cts.Token)        
            

    // Our bound properties
    member x.Name with get() = name.Value and set(v) = name.Value <- v; updateFullName v
    member x.FullName with get() = fullName.Value and set(v) = fullName.Value <- v
    member val Suggestions = System.Collections.ObjectModel.ObservableCollection<string>()

    // Related to progress / status reporting
    member x.Progress = progress

    // Handles lifecycle tracking (when things should be enabled, closed, etc)
    member x.Initialized = initialized.Value    
    member x.RenameComplete = renameComplete.Value

    // The actual commands for the buttons
    member x.ExecuteCommand = execute               
    member x.CancelCommand = cancelCommand
    

// This handles "code behind", ie: pure view logic for our dialog
type RenameDialogViewController() =
    interface FsXaml.IViewController with
        member this.Attach fe =
            // Use the TypeProvider's Accessor sub-type to gain access to named members
            let window = RenameDialog.Accessor fe

            let model = window.Root.DataContext :?> RenameDialogViewModel
            let modelInpc = window.Root.DataContext :?> INotifyPropertyChanged

            // Custom view specific logic to handle keyboard up/down arrow events
            let handler (_: obj) (e: KeyEventArgs) =
                match e.Key with
                | System.Windows.Input.Key.Down ->
                    let current = window.listSuggestions.SelectedIndex
                    window.listSuggestions.SelectedIndex <-
                        match current with
                        | last when last = window.listSuggestions.Items.Count - 1 ->
                            0
                        | other -> 
                            other + 1
                    window.listSuggestions.ScrollIntoView(window.listSuggestions.SelectedItem)
                    e.Handled <- true
                | System.Windows.Input.Key.Up ->
                    let current = window.listSuggestions.SelectedIndex
                    window.listSuggestions.SelectedIndex <-
                        match current with
                        | first when first <= 0  ->
                            window.listSuggestions.Items.Count - 1
                        | other -> 
                            other - 1                    
                    window.listSuggestions.ScrollIntoView(window.listSuggestions.SelectedItem)
                    e.Handled <- true
                | _ -> 
                    ()
            Keyboard.AddPreviewKeyDownHandler(window.Root, KeyEventHandler(handler))

            // Once the model is initialized, focus and select txtName so the user can just type "F2 / new_name / Enter"
            modelInpc.PropertyChanged.Add(fun e ->
                if e.PropertyName = "Initialized" then
                    window.Root.Activate() |> ignore
                    window.txtName.IsEnabled <- true
                    window.txtName.SelectAll()
                    window.txtName.Focus() |> ignore)

            // Map list box suggestion change events to model
            window.listSuggestions.SelectionChanged.Add(fun _ ->
                    if window.listSuggestions.SelectedIndex > -1 then
                        model.Name <- window.listSuggestions.SelectedItem.ToString()
                )

// Module for loading the rename dialog with a viewModel + owner
[<RequireQualifiedAccess>]
module UI =
    let loadRenameDialog (viewModel: RenameDialogViewModel) owner =
        let window = RenameDialog().CreateRoot()
        window.Owner <- owner
        window.DataContext <- viewModel        
        window

