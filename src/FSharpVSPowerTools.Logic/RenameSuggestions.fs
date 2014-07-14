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
    [ let indexMatch = indexRegex.Match(name)
      if indexMatch.Success then
          yield "index" + indexMatch.Groups.[1].Value
      if name = "index" then
          yield "i"
      if name = "i2" || name = "index2" then
          yield "j"
      if name = "i3" || name = "index3" then
          yield "k" ]

let lower (s:string) =
    s.[0].ToString().ToLower() + if s.Length > 1 then s.Substring(1) else ""

let upper (s:string) =
    s.[0].ToString().ToUpper() + if s.Length > 1 then s.Substring(1) else ""

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



let createSuggestions name =    
    use hunspell = new Hunspell("en_us.aff", "en_us.dic")
    let parts = splitInParts name

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

    loop parts
    |> Observable.ofSeq
    |> Observable.map String.Concat
    

let suggest (kind:Kind) (name:string) : IObservable<string> =    
    match kind with
    | Variable -> 
        lower name ::
        suggestIndexNames name
    | Type -> [upper name]        
    |> Observable.ofSeq
    |> Observable.merge (createSuggestions name)
    |> Observable.filter ((<>) name)