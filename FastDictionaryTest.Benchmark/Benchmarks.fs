﻿namespace FastDictionaryTest.Benchmark

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open FastDictionaryTest
open FastDictionaryTest.Benchmark.Domain

[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
type Benchmarks () =

    let rng = Random 123
    let minKey = 0
    let maxKey = 10_000
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
                     let k = 1<Key> * ((rng.Next (minKey, maxKey)) <<< 16)
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

    let testMaps =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Map
            |]
        |]

    let testDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let testReadOnlyDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> ReadOnlyDictionary
            |]
        |]

    let testDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> dict
            |]
        |]

    let testReadOnlyDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> readOnlyDict
            |]
        |]

    let naiveDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> OpenChaining.Dictionary
            |]
        |]

    let zeroAllocDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> ZeroAlloc.Dictionary
            |]
        |]

    let arraysDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Arrays.Dictionary
            |]
        |]

    let embeddedHeadDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> EmbeddedHead.Dictionary
            |]
        |]

    let linearProbingDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> LinearProbing.Dictionary
            |]
        |]

    let cacheHashCodeDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> CacheHashCode.Dictionary
            |]
        |]

    let simdDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Simd.Dictionary
            |]
        |]

    let simd2Dictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Simd2.Dictionary
            |]
        |]

    let monomorphizationDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Monomorphization.Dictionary.ofSeq
            |]
        |]

    let lambdaDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Lambda.Dictionary.ofSeq
            |]
        |]

    let robinHoodDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> RobinHood.Dictionary
            |]
        |]

    let robinHoodSimdDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> RobinHoodSimd.Dictionary
            |]
        |]

    let robinHoodSimdSwitchDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> RobinHoodSimdSwitch.Dictionary
            |]
        |]

    let robinHoodEvictionDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> RobinHoodEviction.Dictionary
            |]
        |]

    let byteListDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> ByteList.Dictionary
            |]
        |]

    [<Params(
          KeyCount.``10``
          , KeyCount.``100``
          , KeyCount.``1_000``
          , KeyCount.``10_000``
          , KeyCount.``MinFill%``
          , KeyCount.``MaxFill%``
        )>]
    member val KeyCount = KeyCount.``10`` with get, set


    // [<Benchmark>]
    member b.Map () =
        let testDataSets = testMaps

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = ".NET Dictionary")>]
    member b.Dictionary () =
        let testDataSets = testDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark>]
    member b.ReadOnlyDictionary () =
        let testDataSets = testReadOnlyDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "dict")>]
    member b.Dict () =
        let testDataSets = testDicts

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "readOnlyDict")>]
    member b.ReadOnlyDict () =
        let testDataSets = testReadOnlyDicts

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Separate Chaining (SC)")>]
    member b.OpenChaining () =
        let testDataSets = naiveDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "SC/No Alloc")>]
    member b.ZeroAllocList () =
        let testDataSets = zeroAllocDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark>]
    member b.Arrays () =
        let testDataSets = arraysDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "SC/Embedded Head")>]
    member b.EmbeddedHead () =
        let testDataSets = embeddedHeadDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Linear Probing (LP)")>]
    member b.LinearProbing () =
        let testDataSets = linearProbingDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "LP/Cache#")>]
    member b.CacheHashCode () =
        let testDataSets = cacheHashCodeDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "LP/Cache#/SIMD")>]
    member b.Simd () =
        let testDataSets = simdDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "LP/Cache#/SIMD/Type Switch")>]
    member b.Simd2 () =
        let testDataSets = simd2Dictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Monomorphization")>]
    member b.Monomorphization () =
        let testDataSets = monomorphizationDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Lambda Monomorphization")>]
    member b.Specialized () =
        let testDataSets = lambdaDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Robin Hood (RH)/Cache#")>]
    member b.RobinHood () =
        let testDataSets = robinHoodDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "RH/Cache#/SIMD")>]
    member b.RobinHoodSimd () =
        let testDataSets = robinHoodSimdDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "RH/Cache#/SIMD/Size Switch")>]
    member b.RobinHoodSimdSwitch () =
        let testDataSets = robinHoodSimdSwitchDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "RH+Eviction")>]
    member b.RobinHoodEviction () =
        let testDataSets = robinHoodEvictionDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = "Byte List")>]
    member b.ByteList () =
        let testDataSets = byteListDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc
