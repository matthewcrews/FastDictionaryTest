open System.Collections.Frozen
open System.Collections.Generic


// type KeyCount =
//     | ``10``       = 0
//     | ``100``      = 1
//     | ``1_000``    = 2
//     | ``10_000``   = 3
//
// let valueCounts = [|
//     KeyCount.``10``       , 10
//     KeyCount.``100``      , 100
//     KeyCount.``1_000``    , 1_000
//     KeyCount.``10_000``   , 10_000
// |]

let rng = System.Random 123
let minKey = -100_000
let maxKey = 100_000
let maxValue = 100_000
let dictionaryCount = 100
let testCount = 100
let keyCount = 100
let lookupCount = 100


let strDataSets =
    [| for _ in 0 .. testCount - 1 ->
         let d = Dictionary()

         while d.Count < dictionaryCount do
             let k = $"Key[{((rng.Next (minKey, maxKey)) <<< 16)}]"
             let v = rng.Next maxValue
             d[k] <- v

         d
         // |> Seq.map (|KeyValue|)
         |> Array.ofSeq
    |]

// Get samples of random keys to look up for each data set
let strKeySets =
    [| for testKey in 0 .. testCount - 1 ->
        let data = strDataSets[testKey]
        [| for _ in 1 .. lookupCount ->
            // Next is exclusive on the upper bound
            data[rng.Next data.Length].Key |]
    |]

let testDictionaries =
    [| for data in strDataSets do
           FrozenDictionary.ToFrozenDictionary(data, optimizeForReading = true) |]

printfn "Starting loops"
for _ in 1 .. 100_000 do

    for i in 0 .. strKeySets.Length - 1 do
        let mutable acc = 0
        let keys = strKeySets[i]
        let d = testDictionaries[i]

        for key in keys do
            acc <- acc + d[key]
