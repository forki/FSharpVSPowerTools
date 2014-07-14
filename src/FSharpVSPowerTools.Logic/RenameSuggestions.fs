module FSharpVSPowerTools.Refactoring.RenameSuggestions

open System.Text.RegularExpressions
open System
open FSharp.Control
open NHunspell

type Kind =
| Type
| Variable

let suggestIndexNames name =
    let indexRegex = Regex("[i](\d*)")
    seq { let indexMatch = indexRegex.Match(name)
          if indexMatch.Success then
              yield "index" + indexMatch.Groups.[1].Value
          if name = "index" then
              yield "i"
          if name = "i2" || name = "index2" then
              yield "j"
          if name = "i3" || name = "index3" then
              yield "k" }
    |> Observable.ofSeq

let lower (s:string) = s.[0].ToString().ToLower() + if s.Length > 1 then s.Substring(1) else ""

let upper (s:string) = s.[0].ToString().ToUpper() + if s.Length > 1 then s.Substring(1) else ""

let splitInParts (name:string) =
    [let last = ref 0
     for i in 0..name.Length-1 do
        if Char.IsUpper(name.[i]) && i <> 0 then
            yield name.Substring(!last,i - !last)
            last := i
        if i = name.Length - 1 then
            yield name.Substring(!last)]

let internal thes = lazy(new MyThes("th_en_us_new.dat"))

let findSynonyms hunspell word =
    let thes = thes.Force()
    let tr = thes.Lookup(word, hunspell)

    [if tr <> null && tr.Meanings <> null then
        for meaning in tr.Meanings do
            if meaning.Synonyms <> null then
                for s in meaning.Synonyms do
                    if s <> word && not (s.Contains " ") && not (s.Contains "-") then                                     
                        if Char.IsLower(word.[0]) then yield lower s else yield upper s]
    |> Set.ofList

let private createSuggestions' name parts =
    let suggestions (observer:IObserver<_>) = async {
        use hunspell = new Hunspell("en_us.aff", "en_us.dic")

        let rec loop parts =
            match parts with
            | [] -> []
            | x::rest ->
                let laterParts = loop rest
                let prepend element =
                    [if laterParts = [] then
                         yield [element]
                     for l in laterParts do
                         yield element :: l]

                if hunspell.Spell x then
                    // word is correct - try to find synonyms
                    [yield! prepend x

                     for w in findSynonyms hunspell x do
                         yield! prepend w]
                else
                    // word is not written correctly try to correct
                    [for s in hunspell.Suggest x do
                         yield! prepend s]


        for s in loop parts do  
            observer.OnNext(s)
        observer.OnCompleted() }


    { new IObservable<_> with
          member x.Subscribe(observer) =
              suggestions observer 
              |> Async.StartDisposable }
    |> Observable.map String.Concat

let createSuggestions name =
    splitInParts name
    |> createSuggestions' name

let getSubParts (name:string) parts =
    let rec getLaterParts parts =
        match parts with
        | [] -> []
        | x::rest ->
            let laterParts = getLaterParts rest
            [if laterParts = [] then
                 yield [x]
             for l in laterParts do
                 yield l
                 yield x :: l]

    getLaterParts parts 
    |> List.map String.Concat 
    |> List.filter (fun s -> name.Contains(s))

let suggest (kind:Kind) (name:string) : IObservable<string> =
    let parts = splitInParts name
    match kind with
    | Variable -> 
        Observable.singleton(name)
        |> Observable.merge (suggestIndexNames name)
    | Type -> 
        Observable.singleton(name)
    |> Observable.merge (Observable.singleton(Pluralizer.toPlural name))
    |> Observable.merge (Observable.singleton(Pluralizer.toSingular name))
    |> Observable.merge (Observable.ofSeq(getSubParts name parts))
    |> Observable.merge (createSuggestions' name parts)
    |> Observable.map (fun x -> if kind = Kind.Variable then lower x else upper x)
    |> Observable.filter ((<>) name)