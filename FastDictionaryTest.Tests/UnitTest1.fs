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

type Value =
    {
        Value : int
    }

let data =
    [for _ in 1..10 ->
        { Value = rng.Next (minKey, maxKey) }, rng.Next maxValue]
    
let expectedValues =
    data
    |> List.map KeyValuePair
    |> Dictionary


[<Test>]
let ``OpenChaining Dictionary matches`` () =
    
    let testDictionary = OpenChaining.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ZeroAlloc Dictionary matches`` () =
    
    let testDictionary = ZeroAlloc.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
        
[<Test>]
let ``Array Dictionary matches`` () =
    
    let testDictionary = Arrays.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``EmbeddedHead Dictionary matches`` () =
    
    let testDictionary = EmbeddedHead.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
        
[<Test>]
let ``LinearProbing Dictionary matches`` () =
    
    let testDictionary = LinearProbing.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
        
[<Test>]
let ``CacheHashCode Dictionary matches`` () =
    
    let testDictionary = CacheHashCode.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``Simd Dictionary matches`` () =
    
    let testDictionary = Simd.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``Simd2 Dictionary matches`` () =
    
    let testDictionary = Simd2.Dictionary data
    
    for k, v in data do
        testDictionary[k] <- v
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``Monomorphization Dictionary matches`` () =
    
    let testDictionary = Monomorphization.Dictionary.ofSeq data
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``Lambda Dictionary matches`` () =
    
    let testDictionary = Lambda.Dictionary.ofSeq data
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``RobinHood Dictionary matches`` () =
    
    let testDictionary = RobinHood.Dictionary data
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``RobinHoodSimd Dictionary matches`` () =
    
    let testDictionary = RobinHoodSimd.Dictionary data
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
        
[<Test>]
let ``RobinHoodSimdSwitch Dictionary matches`` () =
    
    let testDictionary = RobinHoodSimdSwitch.Dictionary data
    
    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)