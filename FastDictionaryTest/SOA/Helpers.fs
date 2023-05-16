module FastDictionaryTest.SOA.Helpers

open System
open System.Numerics
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

[<AbstractClass>]
type StaticDict<'Key, 'Value>() =
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
