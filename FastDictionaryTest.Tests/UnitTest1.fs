module FastDictionaryTest.Tests

open System.Collections.Generic
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()


let data =
    [for i in 1..10 ->
        i * 10, i]
    
let expectedValues =
    data
    |> List.map KeyValuePair
    |> Dictionary


[<Test>]
let ``Naive Dictionary matches`` () =
    
    let naiveDictionary = Naive.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ZeroAlloc Dictionary matches`` () =
    
    let naiveDictionary = ZeroAlloc.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
        
[<Test>]
let ``Array Dictionary matches`` () =
    
    let naiveDictionary = Arrays.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``Chain Dictionary matches`` () =
    
    let naiveDictionary = Next2.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)