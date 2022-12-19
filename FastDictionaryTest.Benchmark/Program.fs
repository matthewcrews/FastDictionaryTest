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
    let mutable result = 0

    match version.ToLower() with
    | "naive" ->
        let b = Naive()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "fibonaccihashing" ->
        let b = FibonacciHashing()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "zeroalloc" ->
        let b = ZeroAlloc()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "arraybuckets" ->
        let b = ArrayBuckets()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "embeddedhead" ->
        let b = EmbeddedHead()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "linearprobing" ->
        let b = LinearProbing()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "cachehashcode" ->
        let b = CacheHashCode()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "robinhood" ->
        let b = RobinHood()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "robinhoodeviction" ->
        let b = RobinHoodEviction()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "bytelist" ->
        let b = ByteList()
        for i in 1 .. loopCount do
            result <- b.Test()

    | "byteliststringcomparer" ->
        let b = ByteListStringComparer()
        for i in 1 .. loopCount do
            result <- b.Test()

    | unknownVersion ->
        failwith $"Unknown version: {unknownVersion}"

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

        | "byteliststringcomparer" ->
            let _ = BenchmarkRunner.Run<ByteListStringComparer>()
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
