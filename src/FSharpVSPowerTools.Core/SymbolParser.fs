﻿namespace FSharpVSPowerTools

open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

type SymbolKind =
| Ident
| Operator
| GenericTypeParameter
| StaticallyResolvedTypeParameter
| Other

type Symbol =
    { Kind: SymbolKind
      Line: int
      LeftColumn: int
      RightColumn: int
      Text: string }
    member x.Range = x.Line, x.LeftColumn, x.Line, x.RightColumn

type DraftToken =
    { Kind: SymbolKind
      Token: TokenInformation 
      RightColumn: int }
    static member Create kind token = 
        { Kind = kind; Token = token; RightColumn = token.LeftColumn + token.FullMatchedLength - 1 }

module SymbolParser =
    /// Get the array of all lex states in current source
    let getLexStates defines (source: string) =
        [|
            /// Iterate through the whole line to get the final lex state
            let rec loop (lineTokenizer: LineTokenizer) lexState =
                match lineTokenizer.ScanToken lexState with
                | None, newLexState -> newLexState
                | Some _, newLexState ->
                    loop lineTokenizer newLexState

            let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")
            let lines = source.Replace("\r\n","\n").Split('\r', '\n')
            let lexState = ref 0L
            for line in lines do 
                yield !lexState
                let lineTokenizer = sourceTokenizer.CreateLineTokenizer line
                lexState := loop lineTokenizer !lexState
        |]

    // Until F.C.S returns lex states of current file, we cache lex states of the current document.
    // We assume that current document will be queried repeatedly
    let queryLexState =
        let currentDocumentState = ref None
        fun source defines line ->
            let lexStates = 
                match !currentDocumentState with
                | Some (lexStates, s, d) when s = source && d = defines ->
                    lexStates
                // OPTIMIZE: if the new document has the current document as a prefix, 
                // we can reuse lexing results and process only the added part.
                | _ ->
                    debug "queryLexState: lexing current document"
                    let lexStates = getLexStates defines source
                    currentDocumentState := Some (lexStates, source, defines) 
                    lexStates
            Debug.Assert(line >= 0 && line < Array.length lexStates, "Should have lex states for every line.")
            lexStates.[line]

    // Returns symbol at a given position.
    let getSymbol source line col lineStr (args: string array): Symbol option =
        let defines =
            args |> Seq.choose (fun s -> if s.StartsWith "--define:" then Some s.[9..] else None)
                 |> Seq.toList
    
        let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")
    
        // get all tokens
        let tokens =
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer lineStr
            let rec loop lexState acc =
                match lineTokenizer.ScanToken lexState with
                | Some tok, state -> loop state (tok :: acc)
                | _ -> List.rev acc
            loop (queryLexState source defines line) []
    
        let isIdentifier t = t.CharClass = TokenCharKind.Identifier
        let isOperator t = t.ColorClass = TokenColorKind.Operator
    
        let (|GenericTypeParameterPrefix|StaticallyResolvedTypeParameterPrefix|Other|) token =
            match token.TokenName with
            | "QUOTE" -> GenericTypeParameterPrefix
            | "INFIX_AT_HAT_OP" -> StaticallyResolvedTypeParameterPrefix
            | _ -> Other
    
        // Operators: Filter out overlapped oparators (>>= operator is tokenized as three distinct tokens: GREATER, GREATER, EQUALS. 
        // Each of them has FullMatchedLength = 3. So, we take the first GREATER and skip the other two).
        //
        // Generic type parameters: we convert QUOTE + IDENT tokens into single IDENT token, altering its LeftColumn 
        // and FullMathedLength (for "'type" which is tokenized as (QUOTE, left=2) + (IDENT, left=3, length=4) 
        // we'll get (IDENT, left=2, lenght=5).
        //
        // Statically resolved type parameters: we convert INFIX_AT_HAT_OP + IDENT tokens into single IDENT token, altering its LeftColumn 
        // and FullMathedLength (for "^type" which is tokenized as (INFIX_AT_HAT_OP, left=2) + (IDENT, left=3, length=4) 
        // we'll get (IDENT, left=2, lenght=5).
        let tokens = 
            tokens
            |> List.fold (fun (acc, lastToken) token ->
                match lastToken with
                | Some t when token.LeftColumn <= t.RightColumn -> acc, lastToken
                | _ ->
                    match token with
                    | GenericTypeParameterPrefix -> acc, Some (DraftToken.Create GenericTypeParameter token)
                    | StaticallyResolvedTypeParameterPrefix -> acc, Some (DraftToken.Create StaticallyResolvedTypeParameter token)
                    | Other ->
                        let draftToken =
                            match lastToken with
                            | Some { Kind = GenericTypeParameter | StaticallyResolvedTypeParameter as kind } when isIdentifier token ->
                                DraftToken.Create kind { token with LeftColumn = token.LeftColumn - 1
                                                                    FullMatchedLength = token.FullMatchedLength + 1 }
                            | _ -> 
                                let kind = if isOperator token then Operator elif isIdentifier token then Ident else Other
                                DraftToken.Create kind token
                        draftToken :: acc, Some draftToken
                ) ([], None)
            |> fst
           
        // One or two tokens that in touch with the cursor (for "let x|(g) = ()" the tokens will be "x" and "(")
        let tokensUnderCursor = tokens |> List.filter (fun x -> x.Token.LeftColumn <= col && x.RightColumn + 1 >= col)
    
        // Select IDENT token. If failes, select OPERATOR token.
        tokensUnderCursor
        |> List.tryFind (fun (x: DraftToken) -> 
            match x.Kind with 
            | Ident | GenericTypeParameter | StaticallyResolvedTypeParameter -> true 
            | _ -> false) 
        |> Option.orElse (List.tryFind (fun (x: DraftToken) -> 
            match x.Kind with 
            | Operator -> true 
            | _ -> false) tokensUnderCursor)
        |> Option.map (fun token ->
            { Kind = token.Kind
              Line = line
              LeftColumn = token.Token.LeftColumn
              RightColumn = token.RightColumn + 1
              Text = lineStr.Substring(token.Token.LeftColumn, token.Token.FullMatchedLength) })
    
    