open System
open System.Collections.Generic
open System.Collections.ObjectModel
open Argu
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FastDictionaryTest

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
    let valueCount = 1000
    let lookupCount = 10_000
    
    let data =
        [| for _ in 1 .. valueCount ->
             { Value = rng.Next (minKey, maxKey) }, rng.Next maxValue |]    
    
    let keys =
        [| for _ in 1 .. lookupCount ->
            // Next is exclusive on the upper bound
            fst data[rng.Next valueCount] |]
    
    let testMap = Map data
    let testDictionary =
        data
        |> Array.map KeyValuePair
        |> Dictionary
    
    let testReadOnlyDictionary =
        data
        |> Array.map KeyValuePair
        |> Dictionary
        |> ReadOnlyDictionary
    
    let testDict = dict data
    let testReadOnlyDict = readOnlyDict data

    let naiveDictionary = OpenChaining.Dictionary data
    let zeroAllocDictionary = ZeroAlloc.Dictionary data
    let arraysDictionary = Arrays.Dictionary data
    let embeddedHeadDictionary = EmbeddedHead.Dictionary data
    let linearProbingDictionary = LinearProbing.Dictionary data
    
    // [<Benchmark>]
    member _.Map () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + testMap[k]

        acc

    [<Benchmark(Description = ".NET Dictionary")>]
    member _.Dictionary () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + testDictionary[k]

        acc
        
    // [<Benchmark>]
    member _.ReadOnlyDictionary () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + testReadOnlyDictionary[k]

        acc
        
    // [<Benchmark(Description = "dict")>]
    member _.Dict () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + testDict[k]

        acc
        
    // [<Benchmark(Description = "readOnlyDict")>]
    member _.ReadOnlyDict () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + testReadOnlyDict[k]

        acc

    [<Benchmark(Description = "Separate Chaining v1")>]
    member _.OpenChaining () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + naiveDictionary[k]

        acc
        
    [<Benchmark(Description = "Separate Chaining v2")>]
    member _.ZeroAllocList () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + zeroAllocDictionary[k]

        acc
        
    // [<Benchmark>]
    member _.Arrays () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + arraysDictionary[k]

        acc
        
    [<Benchmark(Description = "Embedded Head Separate Chaining")>]
    member _.EmbeddedHead () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + embeddedHeadDictionary[k]

        acc
        
    [<Benchmark(Description = "Linear Probing")>]
    member _.LinearProbing () =
        let mutable acc = 0
        
        for k in keys do
            acc <- acc + linearProbingDictionary[k]
    
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