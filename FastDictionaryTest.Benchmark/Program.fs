open Argu
open BenchmarkDotNet.Running
open FastDictionaryTest.Benchmark


[<RequireQualifiedAccess>]
type Args =
    | Task of task: string
    | Benchmark of benchmark: string
    | Iterations of iterations: int
    | Suite of suite: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Task _ -> "Which task to perform. Options: Benchmark or Profile"
            | Benchmark _ -> "Which Benchmark to profile."
            | Iterations _ -> "Number of iterations of the Method to perform for profiling"
            | Suite _ -> "White suite of benchmarks to run"



let profile (version: string) loopCount =

    printfn $"Profiling: {version}, LoopCount: {loopCount}"
    // let b = Benchmarks ()
    let mutable result = 0

    match version.ToLower() with
    // | "dictionary" ->
    //     for i in 1 .. loopCount do
    //         result <- b.Dictionary()
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
    //
    // | "robinhood" ->
    //     for _ in 1 .. loopCount do
    //         result <- b.RobinHood()

    | unknownVersion -> failwith $"Unknown version: {unknownVersion}"

    result


[<EntryPoint>]
let main argv =

    let parser = ArgumentParser.Create<Args> (programName = "Topological Sort")
    let results = parser.Parse argv
    let task = results.GetResult Args.Task

    match task.ToLower() with
    | "benchmark" ->
        let suite = results.GetResult Args.Suite

        match suite.ToLower() with
        | "baseline" ->
            let _ = BenchmarkRunner.Run<Baseline>()
            ()

        | "naive" ->
            let _ = BenchmarkRunner.Run<Naive>()
            ()

        | "fibonaccihashing" ->
            let _ = BenchmarkRunner.Run<FibonacciHashing>()
            ()

        | "zeroalloc" ->
            let _ = BenchmarkRunner.Run<ZeroAlloc>()
            ()

        | "arraybuckets" ->
            let _ = BenchmarkRunner.Run<ArrayBuckets>()
            ()

        | "embeddedhead" ->
            let _ = BenchmarkRunner.Run<EmbeddedHead>()
            ()

        | "linearprobing" ->
            let _ = BenchmarkRunner.Run<LinearProbing>()
            ()

        | "cachehashcode" ->
            let _ = BenchmarkRunner.Run<CacheHashCode>()
            ()

        | "robinhood" ->
            let _ = BenchmarkRunner.Run<RobinHood>()
            ()

        | "robinhoodeviction" ->
            let _ = BenchmarkRunner.Run<RobinHoodEviction>()
            ()

        | "bytelist" ->
            let _ = BenchmarkRunner.Run<ByteList>()
            ()

        | unknownSuite ->
            failwith $"Unknown suite of benchmarks: {unknownSuite}"
        ()

    | "profile" ->
        let method = results.GetResult Args.Benchmark
        let iterations = results.GetResult Args.Iterations
        let _ = profile method iterations
        ()

    | unknownTask -> failwith $"Unknown task: {unknownTask}"

    1
