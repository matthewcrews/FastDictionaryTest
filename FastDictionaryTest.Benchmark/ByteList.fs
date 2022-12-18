﻿namespace FastDictionaryTest.Benchmark


open System.Collections.Generic
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open FastDictionaryTest
open FastDictionaryTest.Benchmark.Domain

[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
type ByteList () =

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
    member val KeyCount = KeyCount.``1_000`` with get, set


    [<Benchmark(Description = "Dictionary")>]
    member b.Dictionary () =
        let testDataSets = dictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[0]
            let keys = keySet[0]

            for k in keys do
                acc <- acc + data[k]

        acc

    [<Benchmark(Description = "Byte List")>]
    member b.Test () =
        let testDataSets = testDictionaries

        let mutable acc = 0
        let dataSets = testDataSets[int b.KeyCount]
        let keySet = keySets[int b.KeyCount]

        for testKey in 0 .. testCount - 1 do
            let data = dataSets[0]
            let keys = keySet[0]

            for k in keys do
                acc <- acc + data[k]

        acc
