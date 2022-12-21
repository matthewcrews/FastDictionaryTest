namespace FastDictionaryTest.Benchmark

open System.Collections.Generic
open System.Collections.ObjectModel
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open FastDictionaryTest.Benchmark.Domain

[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
type Baseline () =

    let intMaps =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> Map
            |]
        |]

    let intDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let readOnlyDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> ReadOnlyDictionary
            |]
        |]

    let dicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> dict
            |]
        |]

    let readOnlyDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> readOnlyDict
            |]
        |]

    [<Params(KeyType.Int, KeyType.String)>]
    member val KeyType = KeyType.Int with get, set

    [<Params(
          KeyCount.``10``
          , KeyCount.``100``
          , KeyCount.``1_000``
          , KeyCount.``10_000``
        )>]
    member val KeyCount = KeyCount.``10`` with get, set


    [<Benchmark(Description = "F# Map")>]
    member b.Map () =
        let mutable acc = 0

        if b.KeyType = KeyType.Int then

            let testDataSets = intMaps

            let dataSet = testDataSets[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

        acc

    [<Benchmark(Description = "Dictionary")>]
    member b.Dictionary () =
        let testDataSets = intDictionaries

        let mutable acc = 0
        let dataSet = testDataSets[int b.KeyCount]
        let keySet = intKeySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSet[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = "ReadOnlyDictionary")>]
    member b.ReadOnlyDictionary () =
        let testDataSets = readOnlyDictionaries

        let mutable acc = 0
        let dataSet = testDataSets[int b.KeyCount]
        let keySet = intKeySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSet[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = "dict")>]
    member b.Dict () =
        let testDataSets = dicts

        let mutable acc = 0
        let dataSet = testDataSets[int b.KeyCount]
        let keySet = intKeySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSet[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = "readOnlyDict")>]
    member b.ReadOnlyDict () =
        let testDataSets = readOnlyDicts

        let mutable acc = 0
        let dataSet = testDataSets[int b.KeyCount]
        let keySet = intKeySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSet[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc
