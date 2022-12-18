module FastDictionaryTest.Benchmark.Domain

open System.Collections.Generic

[<Struct>]
type Key =
    {
        Value : int
    }

type KeyCount =
    | ``10``       = 0
    | ``100``      = 1
    | ``1_000``    = 2
    | ``10_000``   = 3
    | ``MinFill%`` = 4
    | ``MaxFill%`` = 5

let valueCounts = [|
    KeyCount.``10``       , 64
    KeyCount.``100``      , 128
    KeyCount.``1_000``    , 256
    KeyCount.``10_000``   , 1024
    KeyCount.``MaxFill%`` , 380
    KeyCount.``MinFill%`` , 390
|]

let rng = System.Random 123
let minKey = 0
let maxKey = 100_000
let maxValue = 1_000
let lookupCount = 100
let testCount = 100

let dataSets =
    [| for _, count in valueCounts ->
        [| for _ in 0 .. testCount - 1 ->
             let d = Dictionary()

             while d.Count < count do
                 // Make the range of keys brutal for a naive Hashing function for mapping
                 // keys to slots
                 let k = $"Key[{((rng.Next (minKey, maxKey)) <<< 16)}]"
                 let v = rng.Next maxValue
                 d[k] <- v

             d
             |> Seq.map (|KeyValue|)
             |> Array.ofSeq
        |]
    |]

// Get samples of random keys to look up for each data set
let keySets =
    [| for keyCount, count in valueCounts ->
        [| for testKey in 0 .. testCount - 1 ->
            let data = dataSets[int keyCount][testKey]
            [| for _ in 1 .. lookupCount ->
                // Next is exclusive on the upper bound
                fst data[rng.Next data.Length] |]
        |]
    |]
