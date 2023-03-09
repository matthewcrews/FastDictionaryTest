namespace FastDictionaryTest.ByteListRobinHood

open System
open System.Numerics
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

module private Helpers =

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
    // Used for Wrap Around addition/subtraction of offsets
    let mutable wrapAroundMask = buckets.Length - 1

    // This relies on the number of buckets being a power of 2
    let computeHashCode (key: 'Key) =
        if typeof<'Key>.IsValueType then
            EqualityComparer.Default.GetHashCode key
        else
            refComparer.GetHashCode key


    let computeBucketIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let isTail (bucketIdx: int) =
        let homeIdx = computeBucketIndex buckets[bucketIdx].HashCode
        bucketIdx <> homeIdx


    let getParentBucketIdx (hashCode: int) (childBucketIdx: int) =

        let rec loop (ancestorIdx: int) =
            let ancestorBucket = &buckets[ancestorIdx]
            let nextIdx = (ancestorIdx + int ancestorBucket.Next) &&& wrapAroundMask
            if nextIdx = childBucketIdx then
                ancestorIdx
            else
                loop nextIdx

        let initialIdx = computeBucketIndex hashCode
        loop initialIdx


    let distanceFromParent (bucketIdx: int) =
        let bucket = &buckets[bucketIdx]
        let parentIdx = getParentBucketIdx bucket.HashCode bucketIdx
        buckets[parentIdx].Next


    let setBucket next hashCode key value bucketIdx =
        let bucket = &buckets[bucketIdx]
        bucket.Next <- next
        bucket.HashCode <- hashCode
        bucket.Key <- key
        bucket.Value <- value


    let removeFromList (bucketIdx: int) =
        let bucket = &buckets[bucketIdx]
        let parentBucketIdx = getParentBucketIdx bucket.HashCode bucketIdx

        // If this is the Last element in a List, we just need to update the Parent's
        // NextOffset value to 0 and then re-add this entry
        if bucket.IsLast then
            buckets[parentBucketIdx].Next <- 0uy

        // This element is in the middle of the list so we will remove it from the existing
        // list and then re-add it
        else
            buckets[parentBucketIdx].Next <- buckets[parentBucketIdx].Next + bucket.Next

        // Mark the Entry as empty
        bucket.Next <- Next.empty


    let rec addStructEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        let rec insertIntoNextEmptyBucket (parentIdx: int) (hashCode: int) (offset: byte) (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.IsAvailable then
                    setBucket Next.last hashCode key value bucketIdx
                    count <- count + 1
                    buckets[parentIdx].Next <- offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif (isTail bucketIdx) && offset > (distanceFromParent bucketIdx) then
                    // Need to take a temporary copy for the purpose of re-insertion
                    let prevEntry = buckets[bucketIdx]
                    removeFromList bucketIdx
                    setBucket Next.last hashCode key value bucketIdx
                    buckets[parentIdx].Next <- offset
                    addStructEntry prevEntry.HashCode prevEntry.Key prevEntry.Value
                else
                    insertIntoNextEmptyBucket parentIdx hashCode (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket parentIdx hashCode offset 0

        let rec listSearch (hashCode: int) (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]

            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if bucket.HashCode = hashCode && bucket.Key = key then
                bucket.Value <- value

            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif buckets[bucketIdx].IsLast then

                (insertIntoNextEmptyBucket bucketIdx hashCode 1uy (bucketIdx + 1))

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].Next)) &&& wrapAroundMask
                listSearch hashCode nextBucketIdx

        let bucketIdx = computeBucketIndex hashCode
        let bucket = &buckets[bucketIdx]
        // Check if bucket is Empty or a Tombstone
        if bucket.IsAvailable then
            setBucket Next.last hashCode key value bucketIdx
            count <- count + 1

        // If there is already an entry for this Key, overwrite the Value
        elif bucket.HashCode = hashCode && bucket.Key = key then
            bucket.Value <- value

        // Check if the current Entry is part of a chain for a different
        // BucketIdx and should therefore be evicted
        elif isTail bucketIdx then
            // Move the current entry out of this position
            let prevEntry = buckets[bucketIdx]
            removeFromList bucketIdx
            setBucket Next.last hashCode key value bucketIdx
            addStructEntry prevEntry.HashCode prevEntry.Key prevEntry.Value
            count <- count + 1

        // In this case, the current Entry is the head of a list that
        // we need to append to. We start searching for the tail of the list
        else
            listSearch hashCode bucketIdx


    let rec addRefEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        let rec insertIntoNextEmptyBucket (parentIdx: int) (hashCode: int) (offset: byte) (bucketIdx: int) =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.IsAvailable then
                    setBucket Next.last hashCode key value bucketIdx
                    count <- count + 1
                    buckets[parentIdx].Next <- offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif (isTail bucketIdx) && offset > (distanceFromParent bucketIdx) then
                    // Need to take a temporary copy for the purpose of re-insertion
                    let prevEntry = buckets[bucketIdx]
                    removeFromList bucketIdx
                    setBucket Next.last hashCode key value bucketIdx
                    buckets[parentIdx].Next <- offset
                    addRefEntry prevEntry.HashCode prevEntry.Key prevEntry.Value
                else
                    insertIntoNextEmptyBucket parentIdx hashCode (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket parentIdx hashCode offset 0

        let rec listSearch (hashCode: int) (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]

            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if bucket.HashCode = hashCode &&
               refComparer.Equals (bucket.Key, key) then
                bucket.Value <- value

            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif buckets[bucketIdx].IsLast then

                (insertIntoNextEmptyBucket bucketIdx hashCode 1uy (bucketIdx + 1))

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].Next)) &&& wrapAroundMask
                listSearch hashCode nextBucketIdx

        let bucketIdx = computeBucketIndex hashCode
        let bucket = &buckets[bucketIdx]
        // Check if bucket is Empty or a Tombstone
        if bucket.IsAvailable then
            setBucket Next.last hashCode key value bucketIdx
            count <- count + 1

        // If there is already an entry for this Key, overwrite the Value
        elif bucket.HashCode = hashCode &&
             refComparer.Equals (bucket.Key, key) then

            bucket.Value <- value

        // Check if the current Entry is part of a chain for a different
        // BucketIdx and should therefore be evicted
        elif isTail bucketIdx then
            // Move the current entry out of this position
            let prevEntry = buckets[bucketIdx]
            removeFromList bucketIdx
            setBucket Next.last hashCode key value bucketIdx
            addRefEntry prevEntry.HashCode prevEntry.Key prevEntry.Value
            count <- count + 1

        // In this case, the current Entry is the head of a list that
        // we need to append to. We start searching for the tail of the list
        else
            listSearch hashCode bucketIdx


    let getStructValue (key: 'Key) =

        let rec loop (hashCode: int) (bucketIdx: int) =
            let bucket = buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               EqualityComparer.Default.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raise (KeyNotFoundException())

            else
                let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                loop hashCode nextBucketIdx

        let hashCode = EqualityComparer.Default.GetHashCode key
        let bucketIdx = computeBucketIndex hashCode
        loop hashCode bucketIdx


    let getRefValue (key: 'Key) =

        let rec loop (hashCode: int) (bucketIdx: int) =
            let bucket = buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               refComparer.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raise (KeyNotFoundException())

            else
                let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                loop hashCode nextBucketIdx

        let hashCode = refComparer.GetHashCode key
        let bucketIdx = computeBucketIndex hashCode
        loop hashCode bucketIdx


    // Increase the size of the backing array if the max fill percent has been reached
    // and migrate all of the entries.
    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Bucket.empty
            bucketBitShift <- 32 - (BitOperations.TrailingZeroCount buckets.Length)
            wrapAroundMask <- buckets.Length - 1
            count <- 0

            for bucket in oldBuckets do
                if bucket.IsEntry then
                    addStructEntry bucket.HashCode bucket.Key bucket.Value

    do
        for key, value in entries do
            let hashCode = computeHashCode key
            addStructEntry hashCode key value
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) =
            if typeof<'Key>.IsValueType then
                getStructValue key
            else
                getRefValue key
