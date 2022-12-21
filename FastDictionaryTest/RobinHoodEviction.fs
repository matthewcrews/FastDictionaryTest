namespace FastDictionaryTest.RobinHoodEviction

open System
open System.Numerics
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"


module private Helpers =

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

    [<Literal>]
    let POSITIVE_INT_MASK = 0x7FFF_FFFF

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -2
        let tombstone = -1

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Offset : int
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = HashCode.tombstone
        member s.IsEmpty = s.HashCode = HashCode.empty
        member s.IsEntry = s.HashCode >= 0
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0

    module Bucket =

        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Offset = 0
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

    let stringComparer =
        { new IEqualityComparer<string> with
            member _.Equals (a: string, b: string) =
                String.Equals (a, b)

            member _.GetHashCode (a: string) =
                let charSpan = MemoryExtensions.AsSpan a
                let mutable hash1 = (5381u <<< 16) + 5381u
                let mutable hash2 = hash1
                let mutable length = a.Length
                let mutable ptr : nativeptr<uint32> =
                    &&charSpan.GetPinnableReference()
                    |> retype
                while length > 2 do
                    length <- length - 4
                    hash1 <- (BitOperations.RotateLeft (hash1, 5) + hash1) ^^^ (NativePtr.get ptr 0)
                    hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ (NativePtr.get ptr 1)
                    ptr <- NativePtr.add ptr 2

                if length > 0 then
                    hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ (NativePtr.get ptr 0)

                int (hash1 + (hash2 * 1566083941u))
        }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // If the type of 'Key is a ref type, we will want to cache the EqualityComparer
    let refComparer =
        if typeof<'Key>.IsValueType then
            Unchecked.defaultof<_>
        elif typeof<'Key> = typeof<string> then
            stringComparer :?> IEqualityComparer<'Key>
        else
            EqualityComparer<'Key>.Default :> IEqualityComparer<'Key>

    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : Bucket<'Key, 'Value>[] = Array.create 4 Bucket.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)

    // This relies on the number of buckets being a power of 2
    let computeHashCode (key: 'Key) =
        if typeof<'Key>.IsValueType then
            EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK
        else
            refComparer.GetHashCode key &&& POSITIVE_INT_MASK


    let computeBucketIndex (hashCode: int) =
        let hashProduct = uint hashCode * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let rec addEntry (key: 'Key) (value: 'Value) =

        let hashCode = computeHashCode key
        let bucketIdx = computeBucketIndex hashCode

        if typeof<'Key>.IsValueType then

            let rec loop (offset: int) (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    let bucket = &buckets[bucketIdx]
                    // Check if bucket is Empty or a Tombstone
                    if bucket.IsAvailable then
                        bucket.Offset <- offset
                        bucket.HashCode <- hashCode
                        bucket.Key <- key
                        bucket.Value <- value
                        count <- count + 1
                    else
                        // If we reach here, we know the bucket is occupied
                        if hashCode = bucket.HashCode &&
                           EqualityComparer.Default.Equals (key, bucket.Key) then
                            bucket.Value <- value

                        // We will evict the current entry under two conditions
                        // 1. This is the home for the new Entry and the current Entry
                        // is already away from its home
                        // 2. The new Entry is farther from its home than the current Entry
                        elif
                            offset = 0 && bucket.Offset > 0 ||
                            ((bucket.Offset <> 0) && bucket.Offset < offset) then

                            let prevKey = bucket.Key
                            let prevValue = bucket.Value
                            bucket.Offset <- offset
                            bucket.HashCode <- hashCode
                            bucket.Key <- key
                            bucket.Value <- value
                            addEntry prevKey prevValue
                        else
                            loop (offset + 1) hashCode (bucketIdx + 1)
                else
                    // Start over looking from the beginning of the buckets
                    loop (offset + 1) hashCode 0

            loop 0 hashCode bucketIdx

        else

            let rec loop (offset: int) (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    let bucket = &buckets[bucketIdx]
                    // Check if bucket is Empty or a Tombstone
                    if bucket.IsAvailable then
                        bucket.Offset <- offset
                        bucket.HashCode <- hashCode
                        bucket.Key <- key
                        bucket.Value <- value
                        count <- count + 1
                    else
                        // If we reach here, we know the bucket is occupied
                        if hashCode = bucket.HashCode &&
                           refComparer.Equals (key, bucket.Key) then
                            bucket.Value <- value

                        // We will evict the current entry under two conditions
                        // 1. This is the home for the new Entry and the current Entry
                        // is already away from its home
                        // 2. The new Entry is farther from its home than the current Entry
                        elif
                            offset = 0 && bucket.Offset > 0 ||
                            ((bucket.Offset <> 0) && bucket.Offset < offset) then

                            let prevKey = bucket.Key
                            let prevValue = bucket.Value
                            bucket.Offset <- offset
                            bucket.HashCode <- hashCode
                            bucket.Key <- key
                            bucket.Value <- value
                            addEntry prevKey prevValue
                        else
                            loop (offset + 1) hashCode (bucketIdx + 1)
                else
                    // Start over looking from the beginning of the buckets
                    loop (offset + 1) hashCode 0

            loop 0 hashCode bucketIdx


    let getValue (key: 'Key) =

        let hashCode = computeHashCode key
        let bucketIdx = computeBucketIndex hashCode

        if typeof<'Key>.IsValueType then

            let rec loop (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    let bucket = buckets[bucketIdx]
                    if bucket.IsEntry then
                        if hashCode = bucket.HashCode &&
                           EqualityComparer.Default.Equals (key, bucket.Key) then
                            bucket.Value

                        else
                            loop hashCode (bucketIdx + 1)

                    elif bucket.IsTombstone then
                        loop hashCode (bucketIdx + 1)

                    else
                        raise (KeyNotFoundException())
                else
                    loop hashCode 0

            loop hashCode bucketIdx

        else

            let rec loop (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    let bucket = buckets[bucketIdx]
                    if bucket.IsEntry then
                        if hashCode = bucket.HashCode &&
                           refComparer.Equals (key, bucket.Key) then
                            bucket.Value

                        else
                            loop hashCode (bucketIdx + 1)

                    elif bucket.IsTombstone then
                        loop hashCode (bucketIdx + 1)

                    else
                        raise (KeyNotFoundException())
                else
                    loop hashCode 0

            loop hashCode bucketIdx


    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Bucket.empty
            bucketBitShift <- 64 - (BitOperations.TrailingZeroCount buckets.Length)
            count <- 0

            for bucket in oldBuckets do
                if bucket.IsEntry then
                    addEntry bucket.Key bucket.Value

    do
        for k, v in entries do
            addEntry k v
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) = getValue key
