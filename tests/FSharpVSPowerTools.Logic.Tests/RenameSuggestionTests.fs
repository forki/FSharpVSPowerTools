module FSharpVSPowerTools.Logic.Tests.RenameSuggestionTests

open NUnit.Framework
open FSharpVSPowerTools.Refactoring.RenameSuggestions

[<Test>]
let ``should suggest index names``() = 
    suggest Kind.Variable "i" |> shouldContain "index"
    suggest Kind.Variable "i2" |> shouldContain "index2"    
    suggest Kind.Variable "i2452" |> shouldContain "index2452"
    suggest Kind.Variable "i2" |> shouldContain "j"
    suggest Kind.Variable "i3" |> shouldContain "k"
    suggest Kind.Variable "index" |> shouldContain "i"
    suggest Kind.Variable "index2" |> shouldContain "j"    
    suggest Kind.Variable "index3" |> shouldContain "k"

[<Test>]
let ``should suggest lower case variable names``() = 
    suggest Kind.Variable "I" |> shouldContain "i"
    suggest Kind.Variable "Index" |> shouldContain "index"    
    suggest Kind.Variable "MyVariable" |> shouldContain "myVariable"

[<Test>]
let ``should not suggest lower case type names``() = 
    suggest Kind.Type "I" |> shouldNotContain "i"
    suggest Kind.Type "Index" |> shouldNotContain "index"    
    suggest Kind.Type "MyVariable" |> shouldNotContain "myVariable"

[<Test>]
let ``should suggest upper case types``() = 
    suggest Kind.Type "myClass" |> shouldContain "MyClass"
    suggest Kind.Type "book" |> shouldContain "Book"
    suggest Kind.Type "bookIndex" |> shouldContain "BookIndex"

[<Test>]
let ``should not suggest upper case variable names``() = 
    suggest Kind.Variable "i" |> shouldNotContain "I"
    suggest Kind.Variable "index" |> shouldNotContain "Index"    
    suggest Kind.Variable "myVariable" |> shouldNotContain "MyVariable"

[<Test>]
let ``should correct simple type names``() =    
    suggest Kind.Type "Recommendatio" |> shouldContain "Recommendation"
    suggest Kind.Type "Houlse" |> shouldContain "House"

[<Test>]
let ``should correct simple function names``() =    
    suggest Kind.Variable "reccomend" |> shouldContain "recommend"
    suggest Kind.Variable "annalyze" |> shouldContain "analyze"

[<Test>]
let ``should split longer names``() =    
    splitInParts "" |> Seq.isEmpty |> assertTrue 
    splitInParts "RecommendationEngine" |> assertEqual ["Recommendation"; "Engine"]
    splitInParts "HouseOfCards" |> assertEqual ["House"; "Of"; "Cards"]
    splitInParts "RecommenA" |> assertEqual ["Recommen"; "A"]
    splitInParts "analyzeFunctionName" |> assertEqual ["analyze"; "Function"; "Name"]

[<Test>]
let ``should correct longer type names``() =    
    suggest Kind.Type "RecommendatioEngine" |> shouldContain "RecommendationEngine"
    suggest Kind.Type "HoulseOfCards" |> shouldContain "HouseOfCards"

[<Test>]
let ``should create suggestions for function names``() =    
    createSuggestions "annalyze" |> assertEqual ["analyze"; "analyzable"]
    createSuggestions "anallyzeFunctionName" |> shouldContain "analyzeFunctionName"
    createSuggestions "anallyzeFunctionName" |> shouldContain "analyzableFunctionName"
    createSuggestions "getHoulseOfCards" |> shouldContain "getHouseOfCards"

[<Test>]
let ``should correct longer function names``() =    
    suggest Kind.Variable "getMyFrstParamter" |> shouldContain "getMyFirstParameter"
    suggest Kind.Variable "whereIsMyHoulseOfCards" |> shouldContain "whereIsMyHouseOfCards"

[<Test>]
let ``should suggest synonyms for simple types``() =
    suggest Kind.Type "Parameter" |> shouldContain "Constant"
    suggest Kind.Type "Parameter" |> shouldNotContain "Parameter"
    suggest Kind.Type "Parameter" |> shouldContain "Factor"    
    suggest Kind.Type "analyze" |> shouldContain "study"
    suggest Kind.Type "Basket" |> shouldContain "Container"