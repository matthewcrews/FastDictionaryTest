namespace FastDictionaryTest.ByteListRobinHood

open System
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
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
    // Used for Wrap Around addition/subtraction of offsets
    let mutable wrapAroundMask = buckets.Length - 1

    // This relies on the number of buckets being a power of 2
    let computeHashCode (key: 'Key) =
        if typeof<'Key>.IsValueType then
            EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK
        else
            refComparer.GetHashCode key &&& POSITIVE_INT_MASK

    let computeBucketIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let rec addEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        let evict (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]
            let parentBucketIdx = (bucketIdx - (int bucket.PrevOffset)) &&& wrapAroundMask

            // If this is the Last element in a List, we just need to update the Parent's
            // NextOffset value to 0 and then re-add this entry
            if bucket.IsLast then
                buckets[parentBucketIdx].NextOffset <- 0uy
                addEntry bucket.HashCode bucket.Key bucket.Value

            // This element is in the middle of the list so we will remove it from the existing
            // list and then re-add it
            else
                let childBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
                buckets[parentBucketIdx].NextOffset <- buckets[parentBucketIdx].NextOffset + bucket.NextOffset
                buckets[childBucketIdx].PrevOffset <- buckets[childBucketIdx].PrevOffset + bucket.PrevOffset
                addEntry bucket.HashCode bucket.Key bucket.Value

        let rec insertIntoNextEmptyBucket (hashCode: int) (offset: byte) (bucketIdx: int) : byte =
            if bucketIdx < buckets.Length then
                let bucket = &buckets[bucketIdx]
                if bucket.IsAvailable then
                    bucket.PrevOffset <- offset
                    bucket.NextOffset <- 0uy
                    bucket.HashCode <- hashCode
                    bucket.Key <- key
                    bucket.Value <- value
                    count <- count + 1
                    offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif bucket.IsTail && offset > bucket.PrevOffset then
                    evict bucketIdx
                    bucket.PrevOffset <- offset
                    bucket.NextOffset <- 0uy
                    bucket.HashCode <- hashCode
                    bucket.Key <- key
                    bucket.Value <- value
                    count <- count + 1
                    offset
                else
                    insertIntoNextEmptyBucket hashCode (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket hashCode offset 0

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

                buckets[bucketIdx].NextOffset <- byte (insertIntoNextEmptyBucket hashCode 1uy (bucketIdx + 1))

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].NextOffset)) &&& wrapAroundMask
                listSearch hashCode nextBucketIdx

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
        elif bucket.HashCode = hashCode && bucket.Key = key then
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

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let getStructValue (key: 'Key) =

        let rec loop (hashCode: int) (bucketIdx: int) =
            let bucket = buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               EqualityComparer.Default.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raise (KeyNotFoundException())

            else
                let nextBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
                loop hashCode nextBucketIdx

        let hashCode = EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK
        let bucketIdx = computeBucketIndex hashCode
        let bucket = buckets[bucketIdx]

        if hashCode = bucket.HashCode &&
           EqualityComparer.Default.Equals (key, bucket.Key) then
            bucket.Value

        elif bucket.IsLast then
            raise (KeyNotFoundException())

        else
            let nextBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
            loop hashCode nextBucketIdx

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let getRefValue (key: 'Key) =

        let rec loop (hashCode: int) (bucketIdx: int) =
            let bucket = buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               refComparer.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raise (KeyNotFoundException())

            else
                let nextBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
                loop hashCode nextBucketIdx

        let hashCode = refComparer.GetHashCode key &&& POSITIVE_INT_MASK
        let bucketIdx = computeBucketIndex hashCode
        let bucket = buckets[bucketIdx]

        if hashCode = bucket.HashCode &&
           refComparer.Equals (key, bucket.Key) then
            bucket.Value

        elif bucket.IsLast then
            raise (KeyNotFoundException())

        else
            let nextBucketIdx = (bucketIdx + (int bucket.NextOffset)) &&& wrapAroundMask
            loop hashCode nextBucketIdx


    // Increase the size of the backing array if the max fill percent has been reached
    // and migrate all of the entries.
    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Bucket.empty
            bucketBitShift <- 64 - (BitOperations.TrailingZeroCount buckets.Length)
            wrapAroundMask <- buckets.Length - 1
            count <- 0

            for bucket in oldBuckets do
                if bucket.IsEntry then
                    addEntry bucket.HashCode bucket.Key bucket.Value

    do
        for key, value in entries do
            let hashCode = computeHashCode key
            addEntry hashCode key value
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) =
            if typeof<'Key>.IsValueType then
                getStructValue key
            else
                getRefValue key
