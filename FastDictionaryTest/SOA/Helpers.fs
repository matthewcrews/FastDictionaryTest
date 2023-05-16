module FastDictionaryTest.SOA.Helpers

open System
open System.Numerics
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

type IStaticDictionary<'Key, 'Value> =
    abstract member Item : 'Key -> 'Value with get


let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

[<RequireQualifiedAccess>]
module Next =
    let empty = Byte.MaxValue
    let tombstone = Byte.MaxValue - 1uy
    let last = 0uy

    let isTombstone next = next = tombstone
    let isEmpty     next = next = empty
    let isEntry     next = next < tombstone
    let isOccupied  next = next <= tombstone
    let isAvailable next = next >= tombstone
    let isLast      next = next = last


[<Struct>]
type Acc<'Key, 'Value when 'Key : equality> =
    {
        mutable Count: int
        mutable Keys: 'Key[]
        mutable Values: 'Value[]
        mutable HashCodes: int[]
        mutable Nexts: byte[]
        mutable BucketBitShift: int
        mutable WrapAroundMask: int
    }
    static member init () =
        let initialCapacity = 4
        {
            Count = 0
            Keys = Array.zeroCreate 4
            Values = Array.zeroCreate 4
            HashCodes = Array.zeroCreate 4
            Nexts = Array.create 4 Next.empty
            BucketBitShift = 32 - (BitOperations.TrailingZeroCount initialCapacity)
            WrapAroundMask = initialCapacity - 1
        }

[<Struct>]
type Range =
    {
        Start: int
        Length: int
    }
