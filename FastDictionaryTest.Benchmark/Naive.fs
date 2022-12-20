namespace FastDictionaryTest.Benchmark


open System.Collections.Generic
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open FastDictionaryTest
open FastDictionaryTest.Benchmark.Domain

[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
type Naive () =

    let dictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let testDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                dataSets[int countKey][testKey]
                |> Naive.Dictionary
            |]
        |]


    [<Params(
          KeyCount.``10``
          , KeyCount.``100``
          , KeyCount.``1_000``
          , KeyCount.``10_000``
        )>]
    member val KeyCount = KeyCount.``10`` with get, set


    [<Benchmark(Description = "Dictionary")>]
    member b.Dictionary () =
        let testDataSets = dictionaries

        let mutable acc = 0
        let dataSet = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSet[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = "Fibonacci Hashing")>]
    member b.Test () =
        let testDataSets = testDictionaries

        let mutable acc = 0
        let dataSet = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSet[testKey]
            let keys = keySet[testKey]

            for k in keys do
                acc <- acc + data[k]

        acc
