namespace FastDictionaryTest.LinearProbing

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
    let mutable bucketBitShift = 32 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)

    let computeBucketIndex (hashCode: int) =
        let hashProduct = uint hashCode * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let addStructEntry (key: 'Key) (value: 'Value) =

        let rec loop (hashCode: int) (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                // Check if bucket is Empty or a Tombstone
                if bucket.IsAvailable then
                    bucket.HashCode <- hashCode
                    bucket.Key <- key
                    bucket.Value <- value
                    count <- count + 1
                else
                    // If we reach here, we know the bucket is occupied
                    if hashCode = bucket.HashCode &&
                       EqualityComparer.Default.Equals (key, bucket.Key) then
                        bucket.Value <- value
                    else
                        loop hashCode (bucketIdx + 1)
            else
                // Start over looking from the beginning of the buckets
                loop hashCode 0


        let hashCode = EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK
        let bucketIdx = computeBucketIndex hashCode
        loop hashCode bucketIdx


    let addRefEntry (key: 'Key) (value: 'Value) =

        let rec loop (hashCode: int) (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                // Check if bucket is Empty or a Tombstone
                if bucket.IsAvailable then
                    bucket.HashCode <- hashCode
                    bucket.Key <- key
                    bucket.Value <- value
                    count <- count + 1
                else
                    // If we reach here, we know the bucket is occupied
                    if hashCode = bucket.HashCode &&
                       refComparer.Equals (key, bucket.Key) then
                        bucket.Value <- value
                    else
                        loop hashCode (bucketIdx + 1)
            else
                // Start over looking from the beginning of the buckets
                loop hashCode 0


        let hashCode = refComparer.GetHashCode key &&& POSITIVE_INT_MASK
        let bucketIdx = computeBucketIndex hashCode
        loop hashCode bucketIdx


    let addEntry (key: 'Key) (value: 'Value) =

        if typeof<'Key>.IsValueType then
            addStructEntry key value
        else
            addRefEntry key value


    // Use the stored refComparer to reduce overhead in creating new instances
    let getStructValue (key: 'Key) =
        let hashCode = EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK

        let rec loop (bucketIdx: int) =
            // Make sure that we have not gone past the end of the backing array.
            // If we have we will want to continue our loop from the beginning of the array.
            if bucketIdx < buckets.Length then

                // Check that our HashCodes match and the Keys are equivalent
                if hashCode = buckets[bucketIdx].HashCode &&
                   EqualityComparer.Default.Equals (key, buckets[bucketIdx].Key) then
                        buckets[bucketIdx].Value

                // If the Bucket is occupied then we want to move to the next Entry
                elif buckets[bucketIdx].IsOccupied then
                    loop (bucketIdx + 1)

                // If the Bucket is empty then we have failed to find the Key we
                // were searching for
                else
                    raise (KeyNotFoundException())

            else
                // Loop around to the begging of the array
                loop 0

        let bucketIdx = computeBucketIndex hashCode
        loop bucketIdx


    // Use the default GetHashCode to enable the inlining of code for the JIT
    let getRefValue (key: 'Key) =
        let hashCode = refComparer.GetHashCode key &&& POSITIVE_INT_MASK

        let rec loop (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                if hashCode = buckets[bucketIdx].HashCode &&
                   refComparer.Equals (key, buckets[bucketIdx].Key) then
                        buckets[bucketIdx].Value

                elif buckets[bucketIdx].IsOccupied then
                    loop (bucketIdx + 1)

                else
                    raise (KeyNotFoundException())

            else
                // Loop around to the begging of the array
                loop 0

        let bucketIdx = computeBucketIndex hashCode
        loop bucketIdx


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
        with get (key: 'Key) =

            // Switch on the type to enable the JIT to eliminate unused code and inline method call
            if typeof<'Key>.IsValueType then
                getStructValue key

            else
                getRefValue key
