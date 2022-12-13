open System
open System.Collections.Generic
open System.Collections.ObjectModel
open Argu
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FastDictionaryTest


type CountKey =
    | ``10``     = 0
    | ``20``     = 1
    | ``100``    = 2
    | ``200``    = 3
    | ``1_000``  = 4
    | ``2_000``  = 5
    | ``10_000`` = 6
    | ``20_000`` = 7

let valueCounts = [|
    CountKey.``10``     , 10
    CountKey.``20``     , 20
    CountKey.``100``    , 100
    CountKey.``200``    , 200
    CountKey.``1_000``  , 1_000
    CountKey.``2_000``  , 2_000
    CountKey.``10_000`` , 10_000
    CountKey.``20_000`` , 20_000
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
    let lookupCount = 1_000
    
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
            |> Avx.Dictionary
        |]
        
    let avx2Dictionaries =
        [| for countKey, _ in valueCounts ->
            dataSets[int countKey]
            |> Avx2.Dictionary
        |]

    [<Params(
        CountKey.``10``
        , CountKey.``20``
        , CountKey.``100``
        , CountKey.``200``
        , CountKey.``1_000``
        , CountKey.``2_000``
        , CountKey.``10_000``
        , CountKey.``20_000``
        )>]
    member val CountKey = CountKey.``100`` with get, set
        
    // [<Benchmark>]
    member b.Map () =
        let data = testMaps[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    [<Benchmark(Description = ".NET Dictionary")>]
    member b.Dictionary () =
        let data = testDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark>]
    member b.ReadOnlyDictionary () =
        let data = testReadOnlyDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "dict")>]
    member b.Dict () =
        let data = testDicts[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "readOnlyDict")>]
    member b.ReadOnlyDict () =
        let data = testReadOnlyDicts[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Separate Chaining v1")>]
    member b.OpenChaining () =
        let data = naiveDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Separate Chaining v2")>]
    member b.ZeroAllocList () =
        let data = zeroAllocDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark>]
    member b.Arrays () =
        let data = arraysDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Embedded Head Separate Chaining")>]
    member b.EmbeddedHead () =
        let data = embeddedHeadDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    // [<Benchmark(Description = "Linear Probing")>]
    member b.LinearProbing () =
        let data = linearProbingDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc

    [<Benchmark(Description = "Cache HashCode")>]
    member b.CacheHashCode () =
        let data = cacheHashCodeDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    [<Benchmark(Description = "Avx")>]
    member b.Avx () =
        let data = avxDictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + data[k]

        acc
        
    [<Benchmark(Description = "Avx2")>]
    member b.Avx2 () =
        let data = avx2Dictionaries[int b.CountKey]
        let keys = keys[int b.CountKey]
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