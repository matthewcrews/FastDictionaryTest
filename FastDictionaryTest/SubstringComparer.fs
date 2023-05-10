namespace FastDictionaryTest.SubstringComparer

open System
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
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

    let calculateStringIntervals (entries: seq<'Key * 'Value>) =
        let acc = HashSet()

        let calculateUniquenessFactor (start: int) (length: int) (strings: HashSet<string>) =
            acc.Clear()

            for str in strings do
                let strSpan = str.AsSpan (start, length)
                let str = String strSpan
                acc.Add str
                |> ignore

            float acc.Count / (float strings.Count)

        let isNonUniqueTest (start: int) (length: int) (strings: HashSet<string>) =
            acc.Clear()

            for str in strings do
                let strSpan = str.AsSpan (start, length)
                let str = String strSpan
                acc.Add str
                |> ignore

            acc.Count < strings.Count


        let uniqueStrings = HashSet()
        let mutable minLength = Int32.MaxValue
        for key, _ in entries do
            match box key with
            | :? String as strKey ->
                let strSpan = strKey.AsSpan()
                if strSpan.Length < minLength then
                    minLength <- strSpan.Length
                uniqueStrings.Add strKey |> ignore
            | _ -> ()

        // Analyze for which substring provides unique Hashing and Equality
        let minUniquenessFactor = 0.95
        let mutable uniqueness = 0.0
        let mutable hashIndex = 0
        let mutable hashLength = 0

        while uniqueness < minUniquenessFactor && hashLength < minLength do
            hashLength <- hashLength + 1
            hashIndex <- 0

            while uniqueness < minUniquenessFactor &&
                  hashIndex + hashLength < minLength do

                uniqueness <- calculateUniquenessFactor hashIndex hashLength uniqueStrings
                if uniqueness < minUniquenessFactor then
                    hashIndex <- hashIndex + 1


        // let mutable isNonUnique = true
        // let mutable equalityIndex = 0
        // let mutable equalityLength = 0
        //
        // // Equality must be completely unique
        // while isNonUnique do
        //     equalityLength <- equalityLength + 1
        //     equalityIndex <- 0
        //
        //     while isNonUnique && (equalityIndex + equalityLength < minLength) do
        //
        //         if equalityLength < 0 || equalityIndex < 0 then
        //
        //             printfn $"Start: {equalityIndex} Length: {equalityLength}"
        //             printfn "Entries"
        //             for str, value in entries do
        //                 printfn $"{str}, {value}"
        //
        //             printfn "Unique string"
        //             for str in uniqueStrings do
        //                 printfn $"{str}"
        //
        //         isNonUnique <- isNonUniqueTest equalityIndex equalityLength uniqueStrings
        //         if isNonUnique then
        //             equalityIndex <- equalityIndex + 1
        //         else
        //             ()

        // equalityIndex, equalityLength, hashIndex, hashLength
        hashIndex, hashLength

    let createStringComparer (entries: seq<'Key * 'Value>) =
        let hashIndex, hashLength = calculateStringIntervals entries
        { new IEqualityComparer<string> with
            member _.Equals (a: string, b: string) =
                a.AsSpan().SequenceEqual(b.AsSpan())

            member _.GetHashCode (a: string) =
                use ptr = fixed a
                let mutable ptr : nativeptr<UInt64> = retype ptr
                let mutable h = (5381UL <<< 16) + 5381UL
                let mutable length = a.Length

                while length > 0 do
                    h <- 31UL * h + NativePtr.get ptr 0
                    ptr <- NativePtr.add ptr 1

                    length <- length - 4

                int h


                // let mutable hash1 = (5381u <<< 16) + 5381u
                // let mutable hash2 = hash1
                // let mutable length = a.Length
                // use ptr = fixed a
                // let mutable ptr : nativeptr<UInt32> = retype ptr
                // while length > 3 do
                //     hash1 <- (BitOperations.RotateLeft (hash1, 5) + hash1) ^^^ (NativePtr.get ptr 0)
                //     hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ (NativePtr.get ptr 1)
                //     ptr <- NativePtr.add ptr 2
                //     length <- length - 4
                //
                // if length > 0 then
                //     hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ (NativePtr.get ptr 0)
                //
                // int (hash1 + (hash2 * 1_566_083_941u))
        }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // If the type of 'Key is a ref type, we will want to cache the EqualityComparer
    let refComparer =
        if typeof<'Key>.IsValueType then
            Unchecked.defaultof<_>
        elif typeof<'Key> = typeof<string> then
            createStringComparer entries :?> IEqualityComparer<'Key>
        else
            EqualityComparer<'Key>.Default :> IEqualityComparer<'Key>

    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : Bucket<'Key, 'Value>[] = Array.create 4 Bucket.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 32 - (BitOperations.TrailingZeroCount buckets.Length)
    // Used for Wrap Around addition/subtraction of offsets
    let mutable wrapAroundMask = buckets.Length - 1

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
        // list by having the Parent point to the former Grandchild, now child bucket
        else
            buckets[parentBucketIdx].Next <- buckets[parentBucketIdx].Next + bucket.Next


    let rec addStructEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        let rec insertIntoNextEmptyBucket (parentIdx: int) (hashCode: int) (key: 'Key) (value: 'Value) (offset: byte) (bucketIdx: int) =
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
                    insertIntoNextEmptyBucket parentIdx hashCode key value (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket parentIdx hashCode key value offset 0


        let rec listSearch (hashCode: int) (key: 'Key) (value: 'Value) (bucketIdx: int) =
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

                insertIntoNextEmptyBucket bucketIdx hashCode key value 1uy (bucketIdx + 1)

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].Next)) &&& wrapAroundMask
                listSearch hashCode key value nextBucketIdx


        let bucketIdx = computeBucketIndex hashCode
        let bucket = &buckets[bucketIdx]
        // Check if bucket is Empty or a Tombstone
        if bucket.IsAvailable then
            setBucket Next.last hashCode key value bucketIdx
            count <- count + 1

        // If there is already an entry for this Key, overwrite the Value
        elif bucket.HashCode = hashCode &&
            EqualityComparer.Default.Equals (bucket.Key, key) then
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
            listSearch hashCode key value bucketIdx


    let rec addRefEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        let rec insertIntoNextEmptyBucket (parentIdx: int) (hashCode: int) (key: 'Key) (value: 'Value) (offset: byte) (bucketIdx: int) =
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
                    insertIntoNextEmptyBucket parentIdx hashCode key value (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket parentIdx hashCode key value offset 0

        let rec listSearch (hashCode: int) (key: 'Key) (value: 'Value) (bucketIdx: int) =
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

                insertIntoNextEmptyBucket bucketIdx hashCode key value 1uy (bucketIdx + 1)

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int buckets[bucketIdx].Next)) &&& wrapAroundMask
                listSearch hashCode key value nextBucketIdx


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
            listSearch hashCode key value bucketIdx


    let addEntry (key: 'Key) (value: 'Value) =

        if typeof<'Key>.IsValueType then
            let hashCode = EqualityComparer.Default.GetHashCode key
            addStructEntry hashCode key value
        else
            let hashCode = refComparer.GetHashCode key
            addRefEntry hashCode key value


    let raiseKeyNotFound hashcode key =
        printfn $"{refComparer}"
        for bucket in buckets do
            if bucket.IsEntry then
                printfn $"{bucket.Key} {bucket.HashCode}"
        raise (KeyNotFoundException $"Missing Key: {key} Hashcode: {hashcode}")


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let getStructValue (key: 'Key) =

        let rec loop (hashCode: int) (key: 'Key) (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               EqualityComparer.Default.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raiseKeyNotFound hashCode key
            else
                let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                loop hashCode key nextBucketIdx

        let hashCode = EqualityComparer.Default.GetHashCode key
        let bucketIdx = computeBucketIndex hashCode
        let bucket = &buckets[bucketIdx]

        if hashCode = bucket.HashCode &&
           EqualityComparer.Default.Equals (key, bucket.Key) then
            bucket.Value

        elif bucket.IsLast then
            raiseKeyNotFound hashCode key

        else
            let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
            loop hashCode key nextBucketIdx


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let getRefValue (key: 'Key) =

        let rec loop (hashCode: int) (key: 'Key) (bucketIdx: int) =
            let bucket = &buckets[bucketIdx]

            if hashCode = bucket.HashCode &&
               refComparer.Equals (key, bucket.Key) then
                bucket.Value

            elif bucket.IsLast then
                raiseKeyNotFound hashCode key

            else
                let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
                loop hashCode key nextBucketIdx

        let hashCode = refComparer.GetHashCode key
        let bucketIdx = computeBucketIndex hashCode
        let bucket = &buckets[bucketIdx]

        if hashCode = bucket.HashCode &&
           refComparer.Equals (key, bucket.Key) then
            bucket.Value

        elif bucket.IsLast then
            raiseKeyNotFound hashCode key

        else
            let nextBucketIdx = (bucketIdx + (int bucket.Next)) &&& wrapAroundMask
            loop hashCode key nextBucketIdx


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
                    if typeof<'Key>.IsValueType then
                        addStructEntry bucket.HashCode bucket.Key bucket.Value
                    else
                        addRefEntry bucket.HashCode bucket.Key bucket.Value

    do
        for key, value in entries do
            addEntry key value
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) =
            if typeof<'Key>.IsValueType then
                getStructValue key
            else
                getRefValue key
