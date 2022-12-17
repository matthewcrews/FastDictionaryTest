namespace FastDictionaryTest.LinearProbing

open System.Collections.Generic

module private Helpers =

    [<Struct>]
    type Status =
        {
            Value : sbyte
        }
        member s.IsTombstone = s.Value = -1y
        member s.IsEmpty = s.Value = 0y
        member s.IsOccupied = s.Value = 1y
        member s.IsAvailable = s.Value <= 0y
        static member empty = { Value = 0y }
        static member occupied = { Value = 1y }
        static member tombstone = { Value = -1y }

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable Status : Status
            mutable Key : 'Key
            mutable Value : 'Value
        }

    module Bucket =

        let empty<'Key, 'Value> =
            {
                Status = Status.empty
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : Bucket<'Key, 'Value>[] = Array.create 4 Bucket.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)

    // This relies on the number of buckets being a power of 2
    let computeBucketIndex (key: 'Key) =
        let h = EqualityComparer<'Key>.Default.GetHashCode key
        let hashProduct = uint h * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let addEntry (key: 'Key) (value: 'Value) =

        let rec loop (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                // Check if bucket is Empty or a Tombstone
                if bucket.Status.IsAvailable then
                    bucket.Status <- Status.occupied
                    bucket.Key <- key
                    bucket.Value <- value
                    count <- count + 1
                else
                    // If we reach here, we know the bucket is occupied
                    if EqualityComparer.Default.Equals (key, bucket.Key) then
                        bucket.Value <- value
                    else
                        loop (bucketIdx + 1)
            else
                // Start over looking from the beginning of the buckets
                loop 0

        let bucketIdx = computeBucketIndex key
        loop bucketIdx

    let getValue (key: 'Key) =

        let rec loop (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.Status.IsOccupied then
                    if EqualityComparer.Default.Equals (key, bucket.Key) then
                        bucket.Value
                    else
                        loop (bucketIdx + 1)
                elif bucket.Status.IsTombstone then
                    loop (bucketIdx + 1)
                else
                    raise (KeyNotFoundException())
            else
                loop 0

        let bucketIdx = computeBucketIndex key
        loop bucketIdx


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
                if bucket.Status.IsOccupied then
                    addEntry bucket.Key bucket.Value

    do
        for k, v in entries do
            addEntry k v
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) = getValue key

        and set (key: 'Key) (value: 'Value) =
                addEntry key value
                resize()
