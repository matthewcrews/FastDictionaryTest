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
    | ``20``     = 1
    | ``30``     = 2
    | ``40``     = 3
    | ``50``     = 4
    | ``60``     = 5
    | ``70``     = 6
    | ``80``     = 7
    | ``90``     = 8
    | ``100``    = 9
    | ``1_000``    = 10
    | ``2_000``    = 11
    | ``10_000``    = 12
    | ``20_000``    = 13
    
    // | ``100``    = 1
    // | ``1_000``   = 2
    // | ``10_000`` = 3
    //
    // | ``20``   = 1
    // | ``40``   = 2
    // | ``80``   = 3
    // | ``160``  = 4
    // | ``320``  = 5
    // | ``640``  = 6
    // | ``1280`` = 7

type DataType =
    | Int = 0
    | Struct = 1
    | Ref = 2

let valueCounts = [|
    KeyCount.``10``    , 10
    KeyCount.``20``    , 20
    KeyCount.``30``    , 30
    KeyCount.``40``    , 40
    KeyCount.``50``    , 50
    KeyCount.``60``    , 60
    KeyCount.``70``    , 70
    KeyCount.``80``    , 80
    KeyCount.``90``    , 90
    KeyCount.``100``   , 100
    KeyCount.``1_000`` , 1_000
    KeyCount.``2_000`` , 2_000
    KeyCount.``10_000``, 10_000
    KeyCount.``20_000``, 20_000
    
    // KeyCount.``100``   , 100
    // KeyCount.``1_000``   , 1_000
    // KeyCount.``10_000``   , 10_000
    //
    // KeyCount.``20``   , 20
    // KeyCount.``40``   , 40
    // KeyCount.``80``   , 80
    // KeyCount.``160``  , 160
    // KeyCount.``320``  , 320
    // KeyCount.``640``  , 640
    // KeyCount.``1280`` , 1280
|]

// [<Struct>]
type Key = { Value : int }

[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
type Benchmarks () =
    
    let rng = Random 123
    let minKey = -1_000_000_000
    let maxKey = 1_000_000_000
    let maxValue = 1_000_000
    let lookupCount = 100_000
    
    let vFst (struct (a, _)) = a
    let vSnd (struct (_, b)) = b
    
    let dataSets =
        [| for _, count in valueCounts ->
            [| for _ in 1 .. count ->
                 { Value = rng.Next (minKey, maxKey) }, rng.Next maxValue |]
                 // rng.Next (minKey, maxKey), rng.Next maxValue |]
            |> Array.distinctBy fst
        |]
    
    let keys =
        [| for countKey, count in valueCounts ->
            let data = dataSets[int countKey]
            [| for _ in 1 .. lookupCount ->
                // Next is exclusive on the upper bound
                fst data[rng.Next count] |]
        |]
    
    let testMaps =
        [| for countKey, _ in valueCounts ->
            Map dataSets[int countKey]
        |]
        
    let testDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Array.map KeyValuePair
            |> Dictionary
        |]
        
    let testReadOnlyDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Array.map KeyValuePair
            |> Dictionary
            |> ReadOnlyDictionary
        |]
    
    let testDicts =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> dict
        |]
        
    let testReadOnlyDicts =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> readOnlyDict
        |]

    let naiveDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> OpenChaining.Dictionary
        |]

    let zeroAllocDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> ZeroAlloc.Dictionary
        |]

    let arraysDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Arrays.Dictionary
        |]

    let embeddedHeadDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> EmbeddedHead.Dictionary
        |]

    let linearProbingDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> LinearProbing.Dictionary
        |]
    let cacheHashCodeDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> CacheHashCode.Dictionary
        |]
        
    let avxDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Simd.Dictionary
        |]
        
    let avx2Dictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Simd2.Dictionary
        |]
        
    let monomorphizationDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Monomorphization.Dictionary.ofSeq
        |]
        
    let lambdaDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Lambda.Dictionary.ofSeq
        |]
        
    let robinHoodDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> RobinHood.Dictionary
        |]
        
    let robinHoodSimdDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> RobinHoodSimd.Dictionary
        |]

    let robinHoodSimdSwitchDictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> RobinHoodSimdSwitch.Dictionary
        |]

    [<Params(
          KeyCount.``10``
          , KeyCount.``20``
          , KeyCount.``30``
          , KeyCount.``40``
          , KeyCount.``50``
          , KeyCount.``60``
          , KeyCount.``70``
          , KeyCount.``80``
          , KeyCount.``90``
          , KeyCount.``100``
          , KeyCount.``1_000``
          , KeyCount.``2_000``
          , KeyCount.``10_000``
          , KeyCount.``20_000``
          
          // , KeyCount.``100``
          // , KeyCount.``1_000``
          // , KeyCount.``10_000``
          
        // , KeyCount.``20``
        // , KeyCount.``40``
        // , KeyCount.``80``
        // , KeyCount.``160``
        // , KeyCount.``320``
        // , KeyCount.``640``
        // , KeyCount.``1280``

        )>]
    member val KeyCount = KeyCount.``10`` with get, set
        
    // [<Params(
    //       DataType.Int
    //     , DataType.Struct
    //     , DataType.Ref
    //     )>]
    // member val DataType = DataType.Int with get, set
        
    // [<Benchmark>]
    member b.Map () =
        let data = testMaps[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    [<Benchmark(Description = ".NET Dictionary")>]
    member b.Dictionary () =
        let data = testDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark>]
    member b.ReadOnlyDictionary () =
        let data = testReadOnlyDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "dict")>]
    member b.Dict () =
        let data = testDicts[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "readOnlyDict")>]
    member b.ReadOnlyDict () =
        let data = testReadOnlyDicts[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Separate Chaining v1")>]
    member b.OpenChaining () =
        let data = naiveDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Separate Chaining v2")>]
    member b.ZeroAllocList () =
        let data = zeroAllocDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark>]
    member b.Arrays () =
        let data = arraysDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Embedded Head Separate Chaining")>]
    member b.EmbeddedHead () =
        let data = embeddedHeadDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Linear Probing (LP)")>]
    member b.LinearProbing () =
        let data = linearProbingDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    [<Benchmark(Description = "LP + Cache #")>]
    member b.CacheHashCode () =
        let data = cacheHashCodeDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    [<Benchmark(Description = "LP + Cache # + SIMD")>]
    member b.Simd () =
        let data = avxDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    // [<Benchmark(Description = "LP + Cache # + SIMD + Type Switch")>]
    member b.Simd2 () =
        let data = avx2Dictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    // [<Benchmark(Description = "Monomorphization")>]
    member b.Monomorphization () =
        let data = monomorphizationDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    // [<Benchmark(Description = "Lambda Monomorphization")>]
    member b.Specialized () =
        let data = lambdaDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    [<Benchmark(Description = "Robin Hood (RH)")>]
    member b.RobinHood () =
        let data = robinHoodDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    [<Benchmark(Description = "RH + SIMD")>]
    member b.RobinHoodSimd () =
        let data = robinHoodSimdDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "RH + SIMD + Switch")>]
    member b.RobinHoodSimdSwitch () =
        let data = robinHoodSimdSwitchDictionaries[int b.KeyCount]
        let keys = keys[int b.KeyCount]
        let mutable acc = 0
        
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
    | "naive" ->
        for i in 1 .. loopCount do
            result <- b.OpenChaining()

    | "zeroalloc" ->
        for _ in 1 .. loopCount do
            result <- b.ZeroAllocList()
            
    | "embeddedhead" ->
        for _ in 1 .. loopCount do
            result <- b.EmbeddedHead()
            
    | "linearprobing" ->
        for _ in 1 .. loopCount do
            result <- b.LinearProbing()
            
    | "cachehashcode" ->
        for _ in 1 .. loopCount do
            result <- b.CacheHashCode()
        
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