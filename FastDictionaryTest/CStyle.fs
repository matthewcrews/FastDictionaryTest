namespace FastDictionaryTest.CStyle

open System
open System.Numerics
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

module Helpers =

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

    [<RequireQualifiedAccess>]
    module Next =
        let empty = Byte.MaxValue
        let tombstone = Byte.MaxValue - 1uy
        let last = 0uy

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Next : byte
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.Next = Next.tombstone
        member s.IsEmpty = s.Next = Next.empty
        member s.IsEntry = s.Next < Next.tombstone
        member s.IsOccupied = s.Next <= Next.tombstone
        member s.IsAvailable = s.Next >= Next.tombstone
        member s.IsLast = s.Next = Next.last


    module Bucket =

        let empty<'Key, 'Value> =
            {
                HashCode = Unchecked.defaultof<int>
                Next = Next.empty
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

    [<Struct>]
    type Internals<'Key, 'Value> =
        {
            mutable Count : int
            mutable Buckets : Bucket<'Key, 'Value>[]
            mutable BucketBitShift : int
            mutable WrapAroundMask : int
            Comparer : IEqualityComparer<'Key>
        }

    module Internals =

        let empty<'Key, 'Value> : Internals<'Key, 'Value> =
            let initialLength = 4
            // If the type of 'Key is a ref type, we will want to cache the EqualityComparer
            let refComparer =
                if typeof<'Key>.IsValueType then
                    Unchecked.defaultof<_>
                elif typeof<'Key> = typeof<string> then
                    stringComparer :?> IEqualityComparer<'Key>
                else
                    EqualityComparer<'Key>.Default :> IEqualityComparer<'Key>
            {
                Count = 0
                Buckets = Array.create initialLength Bucket.empty
                BucketBitShift = 32 - (BitOperations.TrailingZeroCount initialLength)
                WrapAroundMask = initialLength - 1
                Comparer = refComparer
            }


    let inline computeBucketIndex bucketBitShift (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let isTail (internals: inref<Internals<_,_>>) (bucketIdx: int) =
        let homeIdx = computeBucketIndex internals.BucketBitShift internals.Buckets[bucketIdx].HashCode
        bucketIdx <> homeIdx


    let getParentBucketIdx (internals: inref<Internals<_,_>>) (hashCode: int) (childBucketIdx: int) =
        let buckets = internals.Buckets
        let wrapAroundMask = internals.WrapAroundMask

        let rec loop (ancestorIdx: int) =
            let ancestorBucket = &buckets[ancestorIdx]
            let nextIdx = (ancestorIdx + int ancestorBucket.Next) &&& wrapAroundMask
            if nextIdx = childBucketIdx then
                ancestorIdx
            else
                loop nextIdx

        let initialIdx = computeBucketIndex internals.BucketBitShift hashCode
        loop initialIdx


    let distanceFromParent (internals: inref<Internals<_,_>>) (bucketIdx: int) =
        let bucket = &internals.Buckets[bucketIdx]
        let parentIdx = getParentBucketIdx &internals bucket.HashCode bucketIdx
        internals.Buckets[parentIdx].Next


    let setBucket (bucket: byref<Bucket<_,_>>) next hashCode key value =
        bucket.Next <- next
        bucket.HashCode <- hashCode
        bucket.Key <- key
        bucket.Value <- value


    let removeFromList (internals: byref<Internals<_,_>>) (bucketIdx: int) =
        let buckets = internals.Buckets

        let bucket = &buckets[bucketIdx]
        let parentBucketIdx = getParentBucketIdx &internals bucket.HashCode bucketIdx

        // If this is the Last element in a List, we just need to update the Parent's
        // NextOffset value to 0 and then re-add this entry
        if bucket.IsLast then
            buckets[parentBucketIdx].Next <- 0uy

        // This element is in the middle of the list so we will remove it from the existing
        // list by having the Parent point to the former Grandchild, now child bucket
        else
            buckets[parentBucketIdx].Next <- buckets[parentBucketIdx].Next + bucket.Next


    module rec Struct =

        let rec insertIntoNextEmptyBucket
            (internals: byref<Internals<'Key, 'Value>>)
            (parentIdx: int)
            (hashCode: int)
            (key: 'Key)
            (value: 'Value)
            (offset: byte)
            (bucketIdx: int)
            =

            let buckets = internals.Buckets
            let count = &internals.Count

            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.IsAvailable then
                    setBucket &bucket Next.last hashCode key value
                    count <- count + 1
                    buckets[parentIdx].Next <- offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif (isTail &internals bucketIdx) && offset > (distanceFromParent &internals bucketIdx) then
                    // Need to take a temporary copy for the purpose of re-insertion
                    let prevEntry = buckets[bucketIdx]
                    removeFromList &internals bucketIdx
                    setBucket &bucket Next.last hashCode key value
                    buckets[parentIdx].Next <- offset
                    addEntry &internals prevEntry.HashCode prevEntry.Key prevEntry.Value
                else
                    insertIntoNextEmptyBucket &internals parentIdx hashCode key value (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket &internals parentIdx hashCode key value offset 0


        let rec listSearch
            (internals: byref<Internals<_,_>>)
            (hashCode: int)
            (key: 'Key)
            (value: 'Value)
            (bucketIdx: int)
            =
            let buckets = internals.Buckets
            let wrapAroundMask = internals.WrapAroundMask
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

                insertIntoNextEmptyBucket &internals bucketIdx hashCode key value 1uy (bucketIdx + 1)

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].Next)) &&& wrapAroundMask
                listSearch &internals hashCode key value nextBucketIdx


        let addEntry (internals: byref<Internals<_,_>>) (hashCode: int) (key: 'Key) (value: 'Value) =
            let buckets = internals.Buckets
            let count = &internals.Count

            let bucketIdx = computeBucketIndex internals.BucketBitShift hashCode
            let bucket = &buckets[bucketIdx]
            // Check if bucket is Empty or a Tombstone
            if bucket.IsAvailable then
                setBucket &bucket Next.last hashCode key value
                count <- count + 1

            // If there is already an entry for this Key, overwrite the Value
            elif bucket.HashCode = hashCode &&
                EqualityComparer.Default.Equals (bucket.Key, key) then
                bucket.Value <- value

            // Check if the current Entry is part of a chain for a different
            // BucketIdx and should therefore be evicted
            elif isTail &internals bucketIdx then
                // Move the current entry out of this position
                let prevEntry = buckets[bucketIdx]
                removeFromList &internals bucketIdx
                setBucket &bucket Next.last hashCode key value
                addEntry &internals prevEntry.HashCode prevEntry.Key prevEntry.Value
                count <- count + 1

            // In this case, the current Entry is the head of a list that
            // we need to append to. We start searching for the tail of the list
            else
                listSearch &internals hashCode key value bucketIdx


        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        let getValue
            (internals: inref<Internals<_,_>>)
            (key: 'Key)
            =
            let buckets = internals.Buckets
            let wrapAroundMask = internals.WrapAroundMask
            let bucketBitShift = internals.BucketBitShift

            let rec loop (hashCode: int) (key: 'Key) (bucketIdx: int) =
                let bucket : inref<Bucket<_,_>> = &buckets[bucketIdx]

                if hashCode = bucket.HashCode &&
                   EqualityComparer.Default.Equals (key, bucket.Key) then
                    bucket.Value

                elif bucket.IsLast then
                    raise (KeyNotFoundException())

                else
                    let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                    loop hashCode key nextBucketIdx

            let hashCode = EqualityComparer.Default.GetHashCode key
            let bucketIdx = computeBucketIndex bucketBitShift hashCode
            let bucket : inref<Bucket<_,_>> = &buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               EqualityComparer.Default.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raise (KeyNotFoundException())

            else
                let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                loop hashCode key nextBucketIdx


    module rec Ref =

        let rec insertIntoNextEmptyBucket
            (internals: byref<Internals<_,_>>)
            (parentIdx: int)
            (hashCode: int)
            (key: 'Key)
            (value: 'Value)
            (offset: byte)
            (bucketIdx: int)
            =
            let buckets = internals.Buckets
            let count = &internals.Count

            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.IsAvailable then
                    setBucket &bucket Next.last hashCode key value
                    count <- count + 1
                    buckets[parentIdx].Next <- offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif (isTail &internals bucketIdx) && offset > (distanceFromParent &internals bucketIdx) then
                    // Need to take a temporary copy for the purpose of re-insertion
                    let prevEntry = buckets[bucketIdx]
                    removeFromList &internals bucketIdx
                    setBucket &bucket Next.last hashCode key value
                    buckets[parentIdx].Next <- offset
                    addEntry &internals prevEntry.HashCode prevEntry.Key prevEntry.Value
                else
                    insertIntoNextEmptyBucket &internals parentIdx hashCode key value (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket &internals parentIdx hashCode key value offset 0


        let rec listSearch
            (internals: byref<Internals<_,_>>)
            (hashCode: int)
            (key: 'Key)
            (value: 'Value)
            (bucketIdx: int)
            =
            let wrapAroundMask = internals.WrapAroundMask
            let buckets = internals.Buckets
            let bucket = &buckets[bucketIdx]
            let comparer = internals.Comparer

            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if bucket.HashCode = hashCode &&
               comparer.Equals (bucket.Key, key) then
                bucket.Value <- value

            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif buckets[bucketIdx].IsLast then

                insertIntoNextEmptyBucket &internals bucketIdx hashCode key value 1uy (bucketIdx + 1)

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].Next)) &&& wrapAroundMask
                listSearch &internals hashCode key value nextBucketIdx


        let rec addEntry
            (internals: byref<Internals<'Key,_>>)
            (hashCode: int)
            (key: 'Key)
            (value: 'Value)
            =
            let buckets = internals.Buckets
            let count = &internals.Count
            let comparer = internals.Comparer
            let bucketIdx = computeBucketIndex internals.BucketBitShift hashCode
            let bucket = &buckets[bucketIdx]

            // Check if bucket is Empty or a Tombstone
            if bucket.IsAvailable then
                setBucket &bucket Next.last hashCode key value
                count <- count + 1

            // If there is already an entry for this Key, overwrite the Value
            elif bucket.HashCode = hashCode &&
                comparer.Equals (bucket.Key, key) then
                bucket.Value <- value

            // Check if the current Entry is part of a chain for a different
            // BucketIdx and should therefore be evicted
            elif isTail &internals bucketIdx then
                // Move the current entry out of this position
                let prevEntry = buckets[bucketIdx]
                removeFromList &internals bucketIdx
                setBucket &bucket Next.last hashCode key value
                addEntry &internals prevEntry.HashCode prevEntry.Key prevEntry.Value
                count <- count + 1

            // In this case, the current Entry is the head of a list that
            // we need to append to. We start searching for the tail of the list
            else
                listSearch &internals hashCode key value bucketIdx


        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        let getValue
            (internals: inref<Internals<'Key,_>>)
            (key: 'Key)
            =

            let buckets = internals.Buckets
            let wrapAroundMask = internals.WrapAroundMask
            let bucketBitShift = internals.BucketBitShift
            let comparer = internals.Comparer

            let rec loop (hashCode: int) (key: 'Key) (bucketIdx: int) =
                let bucket : inref<Bucket<_,_>> = &buckets[bucketIdx]

                if hashCode = bucket.HashCode &&
                   comparer.Equals (key, bucket.Key) then
                    bucket.Value

                elif bucket.IsLast then
                    raise (KeyNotFoundException())

                else
                    let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                    loop hashCode key nextBucketIdx

            let hashCode = comparer.GetHashCode key
            let bucketIdx = computeBucketIndex bucketBitShift hashCode
            let bucket : inref<Bucket<_,_>> = &buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               comparer.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raise (KeyNotFoundException())

            else
                let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                loop hashCode key nextBucketIdx



    let addEntry (internals: byref<Internals<_,_>>) (key: 'Key) (value: 'Value) =

        if typeof<'Key>.IsValueType then
            let hashCode = EqualityComparer.Default.GetHashCode key
            Struct.addEntry &internals hashCode key value
        else
            let hashCode = internals.Comparer.GetHashCode key
            Ref.addEntry &internals hashCode key value

    // Increase the size of the backing array if the max fill percent has been reached
    // and migrate all of the entries.
    let resize (internals: byref<Internals<'Key,_>>) =

        // Resize if our fill is >75%
        if internals.Count > (internals.Buckets.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let oldBuckets = internals.Buckets

            // Increase the size of the backing store
            internals.Buckets <- Array.create (internals.Buckets.Length <<< 1) Bucket.empty
            internals.BucketBitShift <- 32 - (BitOperations.TrailingZeroCount internals.Buckets.Length)
            internals.WrapAroundMask <- internals.Buckets.Length - 1
            internals.Count <- 0

            for bucket in oldBuckets do
                if bucket.IsEntry then
                    if typeof<'Key>.IsValueType then
                        Struct.addEntry &internals bucket.HashCode bucket.Key bucket.Value
                    else
                        Ref.addEntry &internals bucket.HashCode bucket.Key bucket.Value


open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    let mutable internals : Internals<'Key, 'Value> = Internals.empty

    do
        for key, value in entries do
            addEntry &internals key value
            resize &internals

    new () = Dictionary<'Key, 'Value>([])

    member b.InternalsByRef = &internals

    member d.Item
        with inline get (key: 'Key) =
            if typeof<'Key>.IsValueType then
                Struct.getValue &d.InternalsByRef key
            else
                Ref.getValue &d.InternalsByRef key
