module FastDictionaryTest.Tests

open System.Collections.Frozen
open System.Collections.Generic
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

type KeyCount =
    | ``10``       = 0
    | ``100``      = 1
    | ``1_000``    = 2
    | ``10_000``   = 3

let valueCounts = [|
    KeyCount.``10``       , 10
    KeyCount.``100``      , 100
    KeyCount.``1_000``    , 1_000
    KeyCount.``10_000``   , 10_000
|]

let rng = System.Random 123
let minKey = -100_000
let maxKey = 100_000
let maxValue = 100_000
let testCount = 100
let lookupCount = 100

let intDataSets =
    [| for _, count in valueCounts ->
        [| for _ in 0 .. testCount - 1 ->
             let d = Dictionary()

             while d.Count < count do
                 let k = rng.Next (minKey, maxKey)
                 // Make the range of keys brutal for a naive Hashing function for mapping keys to slots
                 // let k = ((rng.Next (minKey, maxKey)) <<< 16)
                 let v = rng.Next maxValue
                 d[k] <- v

             d
             |> Seq.map (|KeyValue|)
             |> Array.ofSeq
        |]
    |]

// Get samples of random keys to look up for each data set
let intKeySets =
    [| for keyCount, count in valueCounts ->
        [| for testKey in 0 .. testCount - 1 ->
            let data = intDataSets[int keyCount][testKey]
            [| for _ in 1 .. lookupCount ->
                // Next is exclusive on the upper bound
                fst data[rng.Next data.Length] |]
        |]
    |]

let strDataSets =
    [| for _, count in valueCounts ->
        [| for _ in 0 .. testCount - 1 ->
             let d = Dictionary()

             while d.Count < count do
                 let k = $"Key[{((rng.Next (minKey, maxKey)) <<< 16)}]"
                 let v = rng.Next maxValue
                 d[k] <- v

             d
             |> Seq.map (|KeyValue|)
             |> Array.ofSeq
        |]
    |]

// Get samples of random keys to look up for each data set
let strKeySets =
    [| for keyCount, count in valueCounts ->
        [| for testKey in 0 .. testCount - 1 ->
            let data = strDataSets[int keyCount][testKey]
            [| for _ in 1 .. lookupCount ->
                // Next is exclusive on the upper bound
                fst data[rng.Next data.Length] |]
        |]
    |]


[<Test>]
let ``Naive Dictionary matches`` () =

    (intDataSets, intKeySets)
    ||> Array.iter2 (fun data keys ->
        (data, keys)
        ||> Array.iter2 (fun data keys ->
            let testDictionary = Naive.Dictionary data
            let expectedValues = dict data

            for KeyValue (k, expectedValue) in expectedValues do
                let actualValue = testDictionary[k]
                Assert.AreEqual (expectedValue, actualValue)
            )
        )

//
//
// [<Test>]
// let ``ZeroAlloc Dictionary matches`` () =
//
//     let testDictionary = ZeroAllocation.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``EmbeddedHead Dictionary matches`` () =
//
//     let testDictionary = EmbeddedHead.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``LinearProbing Dictionary matches`` () =
//
//     let testDictionary = LinearProbing.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``RobinHood Dictionary matches`` () =
//
//     let testDictionary = RobinHood.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``RobinHoodEviction Dictionary matches`` () =
//
//     let testDictionary = RobinHoodEviction.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``ByteList Dictionary matches`` () =
//
//     let testDictionary = ByteList.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``ByteList RobinHood Dictionary matches`` () =
//
//     let testDictionary = ByteListRobinHood.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``ByteList RobinHood Inline Dictionary matches`` () =
//
//     let testDictionary = ByteListRobinHoodInline.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//

[<Test>]
let ``SOA Dictionary matches`` () =

    (intDataSets, intKeySets)
    ||> Array.iter2 (fun data keys ->
        (data, keys)
        ||> Array.iter2 (fun data keys ->
            let testDictionary = SOA.StaticDict.create data
            let expectedValues = dict data

            for k in keys do
                let actualValue = testDictionary[k]
                let expectedValue = expectedValues[k]
                Assert.AreEqual (expectedValue, actualValue)
            )
        )

    (strDataSets, strKeySets)
    ||> Array.iter2 (fun data keys ->
        (data, keys)
        ||> Array.iter2 (fun data keys ->
            let testDictionary = SOA.StaticDict.create data
            let expectedValues = dict data

            for k in keys do
                let actualValue = testDictionary[k]
                let expectedValue = expectedValues[k]
                Assert.AreEqual (expectedValue, actualValue)
            )
        )

[<Test>]
let ``Frozen Dictionary matches`` () =

    // (intDataSets, intKeySets)
    // ||> Array.iter2 (fun data keys ->
    //     (data, keys)
    //     ||> Array.iter2 (fun data keys ->
    //         let testDictionary =
    //             data
    //             |> Array.map KeyValuePair
    //             |> Dictionary
    //             |> fun x -> FrozenDictionary.ToFrozenDictionary (x)
    //         let expectedValues = dict data
    //
    //         for k in keys do
    //             let actualValue = testDictionary[k]
    //             let expectedValue = expectedValues[k]
    //             Assert.AreEqual (expectedValue, actualValue)
    //         )
    //     )

    (strDataSets, strKeySets)
    ||> Array.iter2 (fun data keys ->
        (data, keys)
        ||> Array.iter2 (fun data keys ->
            let testDictionary =
                data
                |> Array.map KeyValuePair
                |> Dictionary
                |> fun x -> FrozenDictionary.ToFrozenDictionary (x)
            let expectedValues = dict data

            for k in keys do
                if testDictionary.Count = 100 then
                    ()
                let actualValue = testDictionary[k]
                let expectedValue = expectedValues[k]
                Assert.AreEqual (expectedValue, actualValue)
            )
        )


// [<Test>]
// let ``ByteList RobinHood Vec128 Dictionary matches`` () =
//
//     let testDictionary = ByteListRobinHoodVec128.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
//
//
// [<Test>]
// let ``FastByte Dictionary matches`` () =
//
//     let testDictionary = CStyle.Dictionary data
//
//     for KeyValue (k, expectedValue) in expectedValues do
//         let actualValue = testDictionary[k]
//         Assert.AreEqual (expectedValue, actualValue)
