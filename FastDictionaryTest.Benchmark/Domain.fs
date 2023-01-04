module FastDictionaryTest.Benchmark.Domain

open System.Collections.Generic

type KeyType =
    | Int = 0
    | String = 1

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
let minKey = 0
let maxKey = 100_000
let maxValue = 1_000
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
