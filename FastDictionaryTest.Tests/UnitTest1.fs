module FastDictionaryTest.Tests

open System.Collections.Generic
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

let rng = System.Random 123
let minKey = -1_000_000_000
let maxKey = 1_000_000_000
let maxValue = 1_000_000

let data =
    [for _ in 1..10 ->
        rng.Next (minKey, maxKey), rng.Next maxValue]
    
let expectedValues =
    data
    |> List.map KeyValuePair
    |> Dictionary


[<Test>]
let ``OpenChaining Dictionary matches`` () =
    
    let naiveDictionary = OpenChaining.Dictionary data
    
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
let ``EmbeddedHead Dictionary matches`` () =
    
    let naiveDictionary = EmbeddedHead.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
        
[<Test>]
let ``LinearProbing Dictionary matches`` () =
    
    let naiveDictionary = LinearProbing.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
        
[<Test>]
let ``CacheHashCode Dictionary matches`` () =
    
    let naiveDictionary = CacheHashCode.Dictionary data
    
    for k, v in data do
        naiveDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = naiveDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)