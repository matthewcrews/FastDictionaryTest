open System
open System.Collections.Generic
open System.Collections.ObjectModel
open Argu
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FastDictionaryTest


type KeyCount =
    | ``10``     = 0
    | ``100``    = 1
    | ``1_000``  = 2
    | ``10_000`` = 3

let valueCounts = [|
    KeyCount.``10``    , 10
    KeyCount.``100``   , 100
    KeyCount.``1_000`` , 1_000
    // KeyCount.``10_000``, 10_000
|]

type [<Measure>] Key
type RefKey = { Key : int }

[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
type Benchmarks () =
    
    let rng = Random 123
    let minKey = -1_000_000_000
    let maxKey = 1_000_000_000
    let maxValue = 1_000_000
    let lookupCount = 1_000
    let testCount = 1_000
    
    let dataSets =
        [| for _, count in valueCounts ->
            [| for _ in 0 .. testCount - 1 ->   
                [| for _ in 1 .. count ->
                     // rng.Next (minKey, maxKey) * 1<Key>, rng.Next maxValue |]
                     { Key = rng.Next (minKey, maxKey) } , rng.Next maxValue |]
                |> Array.distinctBy fst
            |]
        |]
    
    // Get samples of random keys to look up for each data set
    let keySets =
        [| for keyCount, count in valueCounts ->
            [| for testKey in 0 .. testCount - 1 ->
                let data = dataSets[int keyCount][testKey]
                [| for _ in 1 .. lookupCount ->
                    // Next is exclusive on the upper bound
                    fst data[rng.Next count] |]
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
    
    [<Params(
          KeyCount.``10``
          , KeyCount.``100``
          , KeyCount.``1_000``
          // , KeyCount.``10_000``
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
    
    [<Benchmark(Description = "LP/Cache#")>]
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
        
    [<Benchmark(Description = "LP/Cache#/SIMD")>]
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
        
    [<Benchmark(Description = "Robin Hood (RH)/Cache#")>]
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
        
    [<Benchmark(Description = "RH/Cache#/SIMD")>]
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

[<RequireQualifiedAccess>]
type Args =
    | Task of task: string
    | Benchmark of benchmark: string
    | Iterations of iterations: int
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Task _ -> "Which task to perform. Options: Benchmark or Profile"
            | Benchmark _ -> "Which Benchmark to profile."
            | Iterations _ -> "Number of iterations of the Method to perform for profiling"


let profile (version: string) loopCount =
    
    printfn $"Profiling: {version}, LoopCount: {loopCount}"
    let b = Benchmarks ()
    let mutable result = 0
    
    match version.ToLower() with
    | "dictionary" ->
        for i in 1 .. loopCount do
            result <- b.Dictionary()
    // | "naive" ->
    //     for i in 1 .. loopCount do
    //         result <- b.OpenChaining()
    //
    // | "zeroalloc" ->
    //     for _ in 1 .. loopCount do
    //         result <- b.ZeroAllocList()
    //         
    // | "embeddedhead" ->
    //     for _ in 1 .. loopCount do
    //         result <- b.EmbeddedHead()
    //         
    // | "linearprobing" ->
    //     for _ in 1 .. loopCount do
    //         result <- b.LinearProbing()
    //         
    // | "cachehashcode" ->
    //     for _ in 1 .. loopCount do
    //         result <- b.CacheHashCode()
        
    | unknownVersion -> failwith $"Unknown version: {unknownVersion}" 
            
    result


[<EntryPoint>]
let main argv =

    printfn $"Args: {argv}"
    
    let parser = ArgumentParser.Create<Args> (programName = "Topological Sort")
    let results = parser.Parse argv
    let task = results.GetResult Args.Task

    match task.ToLower() with
    | "benchmark" -> 
        let _ = BenchmarkRunner.Run<Benchmarks>()
        ()

    | "profile" ->
        let method = results.GetResult Args.Benchmark
        let iterations = results.GetResult Args.Iterations
        let _ = profile method iterations
        ()
        
    | unknownTask -> failwith $"Unknown task: {unknownTask}"
    
    1