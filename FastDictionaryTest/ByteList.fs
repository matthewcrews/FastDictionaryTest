﻿namespace FastDictionaryTest.ByteList

open System.Collections.Generic

module private Helpers =

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -2
        let tombstone = -1

    [<RequireQualifiedAccess>]
    module Offset =
        let head = 0uy
        let last = 0uy

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable PrevOffset : byte
            mutable NextOffset : byte
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = HashCode.tombstone
        member s.IsEmpty = s.HashCode = HashCode.empty
        member s.IsEntry = s.HashCode >= 0
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0
        member s.IsHead = s.PrevOffset = Offset.head
        member s.IsLast = s.NextOffset = Offset.last
        member s.IsTail = s.PrevOffset > 0uy

    module Bucket =

        let empty<'Key, 'Value> =
            {
                HashCode = -1
                PrevOffset = 0uy
                NextOffset = 0uy
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

open Helpers

type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Important to store an instance of comparer for ref types
    let refComparer =
        if typeof<'Key>.IsValueType then
            Unchecked.defaultof<_>
        else
            EqualityComparer<'Key>.Default

    System.Collections.Generic

    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : Bucket<'Key, 'Value>[] = Array.create 4 Bucket.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)
    // Used for Wrap Around addition/subtraction of offsets
    let mutable wrapAroundMask = buckets.Length - 1

    // This relies on the number of buckets being a power of 2. We also make sure the HashCode is positive
    // since we use the top bit to indicate whether the bucket is available. It could either be empty
    // or a tombstone.
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF


    let computeBucketIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let rec addEntry (key: 'Key) (value: 'Value) =

        let rec insertIntoNextEmptyBucket (hashCode: int) (offset: byte) (bucketIdx: int) : byte =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.IsAvailable then
                    bucket.PrevOffset <- offset
                    bucket.NextOffset <- 0uy
                    bucket.HashCode <- hashCode
                    bucket.Key <- key
                    bucket.Value <- value
                    offset
                else
                    insertIntoNextEmptyBucket hashCode (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket hashCode offset 0

        let evict (bucketIdx: int) =
            count <- count - 1
            let bucket = &buckets[bucketIdx]
            let parentBucketIdx = (bucketIdx - (int bucket.PrevOffset)) &&& wrapAroundMask

            // If this is the Last element in a List, we just need to update the Parent's
            // NextOffset value to 0 and then re-add this entry
            if bucket.IsLast then
                buckets[parentBucketIdx].NextOffset <- 0uy
                addEntry bucket.Key bucket.Value

            // This element is in the middle of the list so we will remove it from the existing
            // list and then re-add it
            else
                let childBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
                buckets[parentBucketIdx].NextOffset <- buckets[parentBucketIdx].NextOffset + bucket.NextOffset
                buckets[childBucketIdx].PrevOffset <- buckets[childBucketIdx].PrevOffset + bucket.PrevOffset
                addEntry bucket.Key bucket.Value

        let rec listSearch (hashCode: int) (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]

            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if bucket.HashCode = hashCode &&
                EqualityComparer.Default.Equals (bucket.Key, key) then
                bucket.Value <- value

            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif buckets[bucketIdx].IsLast then

                buckets[bucketIdx].NextOffset <- byte (insertIntoNextEmptyBucket hashCode 1uy (bucketIdx + 1))
                count <- count + 1

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].NextOffset)) &&& wrapAroundMask
                listSearch hashCode nextBucketIdx


        let hashCode = computeHashCode key
        let bucketIdx = computeBucketIndex hashCode
        let bucket = &buckets[bucketIdx]
        // Check if bucket is Empty or a Tombstone
        if bucket.IsAvailable then
            bucket.PrevOffset <- Offset.head
            bucket.NextOffset <- Offset.last
            bucket.HashCode <- hashCode
            bucket.Key <- key
            bucket.Value <- value
            count <- count + 1

        // If there is already an entry for this Key, overwrite the Value
        elif bucket.HashCode = hashCode &&
            EqualityComparer.Default.Equals (bucket.Key, key) then
            bucket.Value <- value

        // Check if the current Entry is part of a chain for a different
        // BucketIdx and should therefore be evicted
        elif bucket.IsTail then
            // Move the current entry out of this position
            evict bucketIdx
            bucket.PrevOffset <- Offset.head
            bucket.NextOffset <- Offset.last
            bucket.HashCode <- hashCode
            bucket.Key <- key
            bucket.Value <- value
            count <- count + 1

        // In this case, the current Entry is the head of a list that
        // we need to append to. We start searching for the tail of the list
        else
            listSearch hashCode bucketIdx


    let getStructValue (key: 'Key) =
        let hashCode = computeHashCode key

        let rec loop (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]
            if hashCode = bucket.HashCode &&
               EqualityComparer.Default.Equals (key, bucket.Key) then
                bucket.Value
            elif bucket.IsLast then
                raise (KeyNotFoundException())
            else
                let nextBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
                loop nextBucketIdx

        let bucketIdx = computeBucketIndex hashCode
        loop bucketIdx


    let getRefValue (key: 'Key) =
        let hashCode = computeHashCode key

        let rec loop (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]
            if hashCode = bucket.HashCode &&
               refComparer.Equals (key, bucket.Key) then
                bucket.Value
            elif bucket.IsLast then
                raise (KeyNotFoundException())
            else
                let nextBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
                loop nextBucketIdx

        let bucketIdx = computeBucketIndex hashCode
        loop bucketIdx


    // Increase the size of the backing array if the max fill percent has been reached
    // and migrate all of the entries.
    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Bucket.empty
            bucketBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)
            wrapAroundMask <- buckets.Length - 1
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
            if typeof<'Key>.IsValueType then
                getStructValue key
            else
                getRefValue key
