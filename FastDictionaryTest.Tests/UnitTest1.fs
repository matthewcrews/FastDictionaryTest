module FastDictionaryTest.Tests

open System.Collections.Generic
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

let rng = System.Random 123
let minKey = -1_000
let maxKey = 1_000
let maxValue = 1_000

type Value =
    {
        Value : int
    }

let data =
    [for _ in 1..12 ->
        { Value = rng.Next (minKey, maxKey) }, rng.Next maxValue]
    |> List.distinctBy fst

let expectedValues =
    data
    |> List.map KeyValuePair
    |> Dictionary


[<Test>]
let ``Naive Dictionary matches`` () =

    let testDictionary = Naive.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ZeroAlloc Dictionary matches`` () =

    let testDictionary = ZeroAllocation.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``EmbeddedHead Dictionary matches`` () =

    let testDictionary = EmbeddedHead.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``LinearProbing Dictionary matches`` () =

    let testDictionary = LinearProbing.Dictionary data

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
let ``RobinHoodEviction Dictionary matches`` () =

    let testDictionary = RobinHoodEviction.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ByteList Dictionary matches`` () =

    let testDictionary = ByteList.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ByteList RobinHood Dictionary matches`` () =

    let testDictionary = ByteListRobinHood.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ByteList RobinHood Inline Dictionary matches`` () =

    let testDictionary = ByteListRobinHoodInline.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)


[<Test>]
let ``ByteList RobinHood Vec128 Dictionary matches`` () =

    let testDictionary = ByteListRobinHoodVec128.Dictionary data

    for KeyValue (k, expectedValue) in expectedValues do
        let actualValue = testDictionary[k]
        Assert.AreEqual (expectedValue, actualValue)
