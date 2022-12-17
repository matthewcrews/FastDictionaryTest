namespace FastDictionaryTest.Simd2

open System.Runtime.Intrinsics
open System.Collections.Generic
open System.Runtime.Intrinsics.X86

#nowarn "42"

module private Domain =

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -1
        let tombstone = -2

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = -1
        member s.IsEmpty = s.HashCode = -2
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0

    module Bucket =

        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

open Domain


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Type flag
    let isStruct = typeof<'Key>.IsValueType
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : Bucket<'Key, 'Value>[] = Array.create 4 Bucket.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)

    // This relies on the number of buckets being a power of 2
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF

    let computeBucketIndex (hashCode: int) =
        let hashProduct = uint hashCode * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let addEntry (key: 'Key) (value: 'Value) =

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
                    if EqualityComparer.Default.Equals (hashCode, bucket.HashCode) &&
                       EqualityComparer.Default.Equals (key, bucket.Key) then
                        bucket.Value <- value
                    else
                        loop hashCode (bucketIdx + 1)
            else
                // Start over looking from the beginning of the buckets
                loop hashCode 0

        let hashCode = computeHashCode key
        let bucketIdx = computeBucketIndex hashCode
        loop hashCode bucketIdx


    let getValue (key: 'Key) =

        let hashCode = computeHashCode key

        let rec loop (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                if EqualityComparer.Default.Equals (hashCode, buckets[bucketIdx].HashCode) &&
                   EqualityComparer.Default.Equals (key, buckets[bucketIdx].Key) then
                       buckets[bucketIdx].Value
                elif buckets[bucketIdx].IsOccupied then
                    loop (bucketIdx + 1)

                else
                    raise (KeyNotFoundException())

            else
                loop 0

        let avxStep (bucketIdx: int) =
            if bucketIdx < buckets.Length - 4 then
                let hashCodeVec = Vector128.Create hashCode
                let bucketsHashCodeVec = Vector128.Create (
                    buckets[bucketIdx].HashCode,
                    buckets[bucketIdx + 1].HashCode,
                    buckets[bucketIdx + 2].HashCode,
                    buckets[bucketIdx + 3].HashCode
                    )

                let compareResult =
                    Sse2.CompareEqual (hashCodeVec, bucketsHashCodeVec)
                    |> retype<_, Vector128<float32>>
                let moveMask = Sse2.MoveMask compareResult
                let offset = System.Numerics.BitOperations.TrailingZeroCount moveMask

                if offset <= 3 &&
                   EqualityComparer.Default.Equals (key, buckets[bucketIdx + offset].Key) then
                    buckets[bucketIdx + offset].Value
                else
                    loop bucketIdx
            else
                loop bucketIdx


        let bucketIdx = computeBucketIndex hashCode
        if isStruct then
            loop bucketIdx
        else
            avxStep bucketIdx


    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Bucket.empty
            bucketBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)
            count <- 0

            for bucket in oldBuckets do
                if bucket.IsOccupied then
                    addEntry bucket.Key bucket.Value

    do
        for k, v in entries do
            addEntry k v
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) = getValue key
