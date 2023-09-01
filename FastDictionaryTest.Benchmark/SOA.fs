namespace FastDictionaryTest.Benchmark

open System.Collections.Frozen
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
type SOA () =

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

    let structDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                structDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let refDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                refDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
            |]
        |]

    let intFrozenDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> fun x -> FrozenDictionary.ToFrozenDictionary(x)
            |]
        |]

    let strFrozenDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> fun x -> FrozenDictionary.ToFrozenDictionary(x)
            |]
        |]

    let structFrozenDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                structDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> fun x -> FrozenDictionary.ToFrozenDictionary(x)
            |]
        |]

    let refFrozenDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                refDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> fun x -> FrozenDictionary.ToFrozenDictionary(x)
            |]
        |]

    let intTestDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> SOA.StaticDict.create
            |]
        |]

    let strTestDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> SOA.StaticDict.create
            |]
        |]

    let structTestDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                structDataSets[int countKey][testKey]
                |> SOA.StaticDict.create
            |]
        |]

    let refTestDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                refDataSets[int countKey][testKey]
                |> SOA.StaticDict.create
            |]
        |]

    // [<Params(KeyType.Int, KeyType.String, KeyType.Struct, KeyType.Ref)>]
    // [<Params(KeyType.Int, KeyType.String)>]
    // [<Params(KeyType.String)>]
    // [<Params(KeyType.Struct)>]
    [<Params(KeyType.Ref)>]
    member val KeyType = KeyType.Int with get, set

    [<Params(
          KeyCount.``10``
          , KeyCount.``100``
          , KeyCount.``1_000``
          , KeyCount.``10_000``
        )>]
    member val KeyCount = KeyCount.``10`` with get, set


    // [<Benchmark(Description = "Dictionary")>]
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

        elif b.KeyType = KeyType.String then
            let mutable acc = 0
            let dataSet = strDictionaries[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        elif b.KeyType = KeyType.Struct then
            let mutable acc = 0
            let dataSet = structDictionaries[int b.KeyCount]
            let keySet = structKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = refDictionaries[int b.KeyCount]
            let keySet = refKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

    [<Benchmark(Description = "Frozen Dictionary")>]
    member b.FrozenDictionary () =

        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intFrozenDictionaries[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        elif b.KeyType = KeyType.String then
            let mutable acc = 0
            let dataSet = strFrozenDictionaries[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        elif b.KeyType = KeyType.Struct then
            let mutable acc = 0
            let dataSet = structFrozenDictionaries[int b.KeyCount]
            let keySet = structKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = refFrozenDictionaries[int b.KeyCount]
            let keySet = refKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc


    [<Benchmark(Description = "Static Dict")>]
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

        elif b.KeyType = KeyType.String then
            let mutable acc = 0
            let dataSet = strTestDictionaries[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        elif b.KeyType = KeyType.Struct then
            let mutable acc = 0
            let dataSet = structTestDictionaries[int b.KeyCount]
            let keySet = structKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = refTestDictionaries[int b.KeyCount]
            let keySet = refKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc
