# Fast Dictionary in F#

This is a repo of my experimentation with writing a Static Dictionary (i.e. ReadOnlyDictionary) with lookup performance that is as fast as possible, with reasonable memory usage. The current fastest version is a Byte-List Separate Chaining with Robin Hood hashing to attempt to keep Keys that hash to the same bucket are as close as possible.

Most of the ideas used here come from a CppCon talk given in 2018 by [Malte Skarupke](https://probablydance.com/) titled [C++Now 2018: You Can Do Better than std::unordered_map: New Improvements to Hash Table Performance](https://www.youtube.com/watch?v=M2fKMP47slQ&t=136s). This repo is my attempt to implement several of the ideas in F# to see what performance could be achieved.

## Results

Here are the results for the current best performing `Dictionary` version.

```
// * Summary *

BenchmarkDotNet=v0.13.2, OS=Windows 11 (10.0.22621.963)
AMD Ryzen 9 3900X, 1 CPU, 24 logical and 12 physical cores
.NET SDK=6.0.402
  [Host]     : .NET 6.0.10 (6.0.1022.47605), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.10 (6.0.1022.47605), X64 RyuJIT AVX2
```

|               Method | KeyType | KeyCount |      Mean |     Error |    StdDev | BranchInstructions/Op | BranchMispredictions/Op | CacheMisses/Op | Allocated |
|--------------------- |-------- |--------- |----------:|----------:|----------:|----------------------:|------------------------:|---------------:|----------:|
|           Dictionary |     Int |       10 |  5.069 us | 0.0799 us | 0.0820 us |                11,898 |                      51 |              5 |         - |
| 'ByteList RobinHood' |     Int |       10 |  3.599 us | 0.0508 us | 0.0475 us |                 8,548 |                      11 |              6 |         - |
|           Dictionary |     Int |      100 |  5.409 us | 0.0317 us | 0.0281 us |                12,290 |                      37 |              5 |         - |
| 'ByteList RobinHood' |     Int |      100 |  3.437 us | 0.0289 us | 0.0270 us |                 7,721 |                      42 |              5 |         - |
|           Dictionary |     Int |    1_000 |  6.070 us | 0.1193 us | 0.1277 us |                12,270 |                      29 |             13 |         - |
| 'ByteList RobinHood' |     Int |    1_000 |  3.738 us | 0.0559 us | 0.0801 us |                 8,068 |                      26 |             12 |         - |
|           Dictionary |     Int |   10_000 | 18.431 us | 0.3551 us | 0.4617 us |                12,569 |                      46 |          2,014 |         - |
| 'ByteList RobinHood' |     Int |   10_000 |  3.753 us | 0.0182 us | 0.0162 us |                 7,999 |                      22 |              7 |         - |
|           Dictionary |  String |       10 | 17.499 us | 0.2165 us | 0.2025 us |                36,514 |                     533 |             19 |         - |
| 'ByteList RobinHood' |  String |       10 | 14.645 us | 0.1842 us | 0.1538 us |                27,544 |                     362 |             22 |         - |
|           Dictionary |  String |      100 | 18.580 us | 0.2062 us | 0.1929 us |                36,878 |                     595 |             21 |         - |
| 'ByteList RobinHood' |  String |      100 | 14.966 us | 0.1430 us | 0.1338 us |                27,379 |                     389 |             19 |         - |
|           Dictionary |  String |    1_000 | 22.116 us | 0.4364 us | 0.4481 us |                36,972 |                     602 |          1,211 |         - |
| 'ByteList RobinHood' |  String |    1_000 | 17.685 us | 0.3533 us | 0.3305 us |                27,771 |                     407 |            488 |         - |
|           Dictionary |  String |   10_000 | 27.625 us | 0.4776 us | 0.4467 us |                37,149 |                     610 |          2,228 |         - |
| 'ByteList RobinHood' |  String |   10_000 | 19.450 us | 0.1639 us | 0.1368 us |                27,934 |                     409 |            692 |         - |

## Running Benchmarks

To run the benchmakrs for yourself, open a Terminal instance with Admin privledges so that you can get the hardware counter results. Navigate your terminal to the `FastDictionaryTest.Benchmarks` direction. The following command you will want to use is:

```
dotnet run -c Release --task benchmark --suite <Suite of Interest>
```

The possible values for `<Suite of Interest>` are:
- Baseline
- Naive
- ZeroAllocation
- FibonacciHashing
- CacheEquality
- FastTypeBranch
- EmbeddedHead
- LinearProbing
- RobinHood
- RobinhoodEviction
- ByteList
- ByteListRobinHood
- ByteListRobinHoodVec128