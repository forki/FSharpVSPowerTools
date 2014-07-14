module FSharpVSPowerTools.Logic.Tests.RenameSuggestionTests

open NUnit.Framework
open FSharpVSPowerTools.Refactoring.RenameSuggestions

[<Test>]
let ``should suggest index names``() = 
    suggest Kind.Variable "i" |> shouldObserve "index"
    suggest Kind.Variable "i2" |> shouldObserve "index2"    
    suggest Kind.Variable "i2452" |> shouldObserve "index2452"
    suggest Kind.Variable "i2" |> shouldObserve "j"
    suggest Kind.Variable "i3" |> shouldObserve "k"
    suggest Kind.Variable "index" |> shouldObserve "i"
    suggest Kind.Variable "index2" |> shouldObserve "j"    
    suggest Kind.Variable "index3" |> shouldObserve "k"

[<Test>]
let ``should suggest lower case variable names``() = 
    suggest Kind.Variable "I" |> shouldObserve "i"
    suggest Kind.Variable "Index" |> shouldObserve "index"    
    suggest Kind.Variable "MyVariable" |> shouldObserve "myVariable"

[<Test>]
let ``should not suggest lower case type names``() = 
    suggest Kind.Type "I" |> shouldNotObserve "i"
    suggest Kind.Type "Index" |> shouldNotObserve "index"    
    suggest Kind.Type "MyVariable" |> shouldNotObserve "myVariable"

[<Test>]
let ``should suggest upper case types``() = 
    suggest Kind.Type "myClass" |> shouldObserve "MyClass"
    suggest Kind.Type "book" |> shouldObserve "Book"
    suggest Kind.Type "bookIndex" |> shouldObserve "BookIndex"

[<Test>]
let ``should not suggest upper case variable names``() = 
    suggest Kind.Variable "i" |> shouldNotObserve "I"
    suggest Kind.Variable "index" |> shouldNotObserve "Index"    
    suggest Kind.Variable "myVariable" |> shouldNotObserve "MyVariable"

[<Test>]
let ``should correct simple type names``() =    
    suggest Kind.Type "Recommendatio" |> shouldObserve "Recommendation"
    suggest Kind.Type "Houlse" |> shouldObserve "House"

[<Test>]
let ``should correct simple function names``() =    
    suggest Kind.Variable "reccomend" |> shouldObserve "recommend"
    suggest Kind.Variable "annalyze" |> shouldObserve "analyze"

[<Test>]
let ``should split longer names``() =    
    splitInParts "" |> Seq.isEmpty |> assertTrue 
    splitInParts "RecommendationEngine" |> assertEqual ["Recommendation"; "Engine"]
    splitInParts "HouseOfCards" |> assertEqual ["House"; "Of"; "Cards"]
    splitInParts "RecommenA" |> assertEqual ["Recommen"; "A"]
    splitInParts "analyzeFunctionName" |> assertEqual ["analyze"; "Function"; "Name"]

[<Test>]
let ``should correct longer type names``() =    
    suggest Kind.Type "RecommendatioEngine" |> shouldObserve "RecommendationEngine"
    suggest Kind.Type "HoulseOfCards" |> shouldObserve "HouseOfCards"

[<Test>]
let ``should create suggestions for function names``() =    
    createSuggestions "annalyze" |> shouldObserve "analyze"
    createSuggestions "annalyze" |> shouldObserve "analyzable"
    createSuggestions "anallyzeFunctionName" |> shouldObserve "analyzeFunctionName"
    createSuggestions "anallyzeFunctionName" |> shouldObserve "analyzableFunctionName"
    createSuggestions "getHoulseOfCards" |> shouldObserve "getHouseOfCards"

[<Test>]
let ``should correct longer function names``() =    
    suggest Kind.Variable "getMyFrstParamter" |> shouldObserve "getMyFirstParameter"
    suggest Kind.Variable "whereIsMyHoulseOfCards" |> shouldObserve "whereIsMyHouseOfCards"

[<Test>]
let ``should suggest synonyms for simple types``() =
    suggest Kind.Type "Parameter" |> shouldObserve "Constant"
    suggest Kind.Type "Parameter" |> shouldNotObserve "Parameter"
    suggest Kind.Type "Parameter" |> shouldObserve "Factor"    
    suggest Kind.Type "analyze" |> shouldObserve "study"
    suggest Kind.Type "Basket" |> shouldObserve "Container"