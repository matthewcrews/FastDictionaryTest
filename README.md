# Fast Dictionary in F#

This is a repo of my experimentation with writing a Static Dictionary (i.e. ReadOnlyDictionary) with lookup performance that is as fast as possible, with reasonable memory usage. The current fastest version is a Byte-List Separate Chaining with Robin Hood hashing to attempt to keep Keys that hash to the same bucket are as close as possible.

Most of the ideas used here come from a CppCon talk given in 2018 by [Malte Skarupke](https://probablydance.com/) titled [C++Now 2018: You Can Do Better than std::unordered_map: New Improvements to Hash Table Performance](https://www.youtube.com/watch?v=M2fKMP47slQ&t=136s). This repo is my attempt to implement several of the ideas in F# to see what performance could be achieved.

## Results

Here are the results for the current best performing `Dictionary` version.

```
// * Summary *

BenchmarkDotNet=v0.13.2, OS=Windows 11 (10.0.22621.1702)
13th Gen Intel Core i9-13900K, 1 CPU, 32 logical and 24 physical cores
.NET SDK=8.0.100-preview.3.23178.7
  [Host]     : .NET 8.0.0 (8.0.23.17408), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 8.0.0 (8.0.23.17408), X64 RyuJIT AVX2
```

|              Method | KeyType | KeyCount |      Mean |    Error |   StdDev | Code Size | BranchInstructions/Op | CacheMisses/Op | BranchMispredictions/Op | Allocated |
|-------------------- |-------- |--------- |----------:|---------:|---------:|----------:|----------------------:|---------------:|------------------------:|----------:|
|          Dictionary |     Int |       10 |  55.26 us | 0.584 us | 0.546 us |   1,566 B |               113,966 |             86 |                   2,983 |         - |
| 'Frozen Dictionary' |     Int |       10 | 111.99 us | 0.294 us | 0.260 us |   1,247 B |               338,890 |             25 |                   8,779 |         - |
|       'Static Dict' |     Int |       10 |  21.47 us | 0.147 us | 0.130 us |   1,129 B |                75,423 |             20 |                     974 |         - |
|          Dictionary |     Int |      100 |  66.01 us | 0.505 us | 0.473 us |   1,566 B |               115,166 |             39 |                   3,580 |         - |
| 'Frozen Dictionary' |     Int |      100 |  23.14 us | 0.241 us | 0.201 us |   1,247 B |               111,234 |              4 |                     344 |         - |
|       'Static Dict' |     Int |      100 |  16.45 us | 0.314 us | 0.374 us |   1,129 B |                69,543 |              3 |                     470 |         - |
|          Dictionary |     Int |    1_000 |  66.22 us | 0.205 us | 0.182 us |   1,566 B |               114,626 |             38 |                   3,454 |         - |
| 'Frozen Dictionary' |     Int |    1_000 |  33.35 us | 0.368 us | 0.326 us |   1,247 B |               114,353 |             33 |                     880 |         - |
|       'Static Dict' |     Int |    1_000 |  21.54 us | 0.132 us | 0.117 us |   1,129 B |                72,319 |             10 |                     823 |         - |
|          Dictionary |     Int |   10_000 | 109.51 us | 1.246 us | 1.104 us |   1,566 B |               116,149 |          2,081 |                   3,761 |         - |
| 'Frozen Dictionary' |     Int |   10_000 |  67.62 us | 1.309 us | 1.161 us |   1,247 B |               114,929 |          2,527 |                     778 |         - |
|       'Static Dict' |     Int |   10_000 |  44.55 us | 0.871 us | 0.855 us |   1,129 B |                74,966 |            945 |                   1,476 |         - |
|          Dictionary |  String |       10 | 170.37 us | 1.455 us | 1.215 us |   1,551 B |               444,723 |             27 |                   8,670 |         - |
| 'Frozen Dictionary' |  String |       10 | 130.26 us | 1.043 us | 0.925 us |   1,247 B |               417,279 |             21 |                   7,581 |         - |
|       'Static Dict' |  String |       10 |  90.23 us | 0.687 us | 0.536 us |   1,129 B |               325,809 |             15 |                   3,841 |         - |
|          Dictionary |  String |      100 | 185.48 us | 2.765 us | 2.587 us |   1,551 B |               446,424 |             77 |                   9,919 |         - |
| 'Frozen Dictionary' |  String |      100 |  98.59 us | 1.119 us | 1.047 us |   1,247 B |               376,587 |             59 |                   3,083 |         - |
|       'Static Dict' |  String |      100 |  92.69 us | 1.137 us | 1.063 us |   1,129 B |               320,451 |             29 |                   3,539 |         - |
|          Dictionary |  String |    1_000 | 247.58 us | 3.099 us | 2.898 us |   1,551 B |               447,108 |          1,523 |                   9,563 |         - |
| 'Frozen Dictionary' |  String |    1_000 | 153.22 us | 1.506 us | 1.335 us |   1,247 B |               357,688 |          1,861 |                   2,888 |         - |
|       'Static Dict' |  String |    1_000 | 137.71 us | 1.561 us | 1.460 us |   1,129 B |               325,975 |            646 |                   3,944 |         - |
|          Dictionary |  String |   10_000 | 365.60 us | 6.803 us | 6.031 us |   1,551 B |               450,305 |          6,617 |                  10,426 |         - |
| 'Frozen Dictionary' |  String |   10_000 | 245.21 us | 2.819 us | 2.354 us |   1,247 B |               383,241 |          7,225 |                   4,193 |         - |
|       'Static Dict' |  String |   10_000 | 197.43 us | 1.163 us | 1.031 us |   1,129 B |               332,395 |          6,683 |                   5,179 |         - |

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
- ByteListRobinHoodInline
- CStyle
- SOA