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

    let intReadOnlyDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> ReadOnlyDictionary
            |]
        |]

    let intDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> dict
            |]
        |]

    let intReadOnlyDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                intDataSets[int countKey][testKey]
                |> readOnlyDict
            |]
        |]

    let strMaps =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> Map
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

    let strReadOnlyDictionaries =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> Array.map KeyValuePair
                |> Dictionary
                |> ReadOnlyDictionary
            |]
        |]

    let strDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
                |> dict
            |]
        |]

    let strReadOnlyDicts =
        [| for countKey, _ in valueCounts ->
            [|for testKey in 0 .. testCount - 1 ->
                strDataSets[int countKey][testKey]
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

        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intMaps[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = strMaps[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

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

    [<Benchmark(Description = "ReadOnlyDictionary")>]
    member b.ReadOnlyDictionary () =
        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intReadOnlyDictionaries[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = strReadOnlyDictionaries[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

    [<Benchmark(Description = "dict")>]
    member b.Dict () =
        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intDicts[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = strDicts[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

    [<Benchmark(Description = "readOnlyDict")>]
    member b.ReadOnlyDict () =
        if b.KeyType = KeyType.Int then
            let mutable acc = 0
            let dataSet = intReadOnlyDicts[int b.KeyCount]
            let keySet = intKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc

        else
            let mutable acc = 0
            let dataSet = strReadOnlyDicts[int b.KeyCount]
            let keySet = strKeySets[int b.KeyCount]

            for testKey in 0 .. testCount - 1 do
                let data = dataSet[testKey]
                let keys = keySet[testKey]

                for k in keys do
                    acc <- acc + data[k]

            acc
