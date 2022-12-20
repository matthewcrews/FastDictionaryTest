module FastDictionaryTest.Benchmark.Domain

open System.Collections.Generic

[<Struct>]
type StructKey =
    {
        Value : int
    }

type RefKey =
    {
        Value : int
    }

type KeyCount =
    | ``10``       = 0
    | ``20``       = 1
    | ``100``      = 2
    | ``200``      = 3
    | ``1_000``    = 4
    | ``2_000``    = 5
    | ``10_000``   = 6
    | ``20_000``   = 7
    | ``MaxFill%`` = 8
    | ``MinFill%`` = 9

let valueCounts = [|
    KeyCount.``10``       , 10
    KeyCount.``20``       , 20
    KeyCount.``100``      , 100
    KeyCount.``200``      , 200
    KeyCount.``1_000``    , 1_000
    KeyCount.``2_000``    , 2_000
    KeyCount.``10_000``   , 10_000
    KeyCount.``20_000``   , 20_000
    KeyCount.``MaxFill%`` , 190
    KeyCount.``MinFill%`` , 194
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
                 let k = rng.Next (minKey, maxKey)
                 // let k = ((rng.Next (minKey, maxKey)) <<< 16)
                 // let k : StructKey = { Value = ((rng.Next (minKey, maxKey)) <<< 16) }
                 // let k : RefKey = { Value = ((rng.Next (minKey, maxKey)) <<< 16) }
                 // let k = $"Key[{((rng.Next (minKey, maxKey)) <<< 16)}]"
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
