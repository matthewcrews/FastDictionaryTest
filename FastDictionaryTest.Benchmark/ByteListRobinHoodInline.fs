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
[<DisassemblyDiagnoser(filters=[||])>]
type ByteListRobinHoodInline () =

    let intDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let strDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let intTestDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> ByteListRobinHoodInline.Dictionary
            |]
        |]

    let strTestDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> ByteListRobinHoodInline.Dictionary
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


    [<Benchmark(Description = "Dictionary")>]
    member b.Dictionary () =

        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intDictionaries[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = strDictionaries[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc


    [<Benchmark(Description = "ByteList RobinHood Inline")>]
    member b.Test () =

        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intTestDictionaries[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = strTestDictionaries[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc
