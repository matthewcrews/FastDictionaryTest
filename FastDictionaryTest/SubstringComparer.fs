namespace FastDictionaryTest.SubstringComparer

open System
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

type IStaticDictionary<'Key, 'Value> =
    abstract member Item : 'Key -> 'Value with get

module private Helpers =

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


    [<Struct>]
    type Data<'Key, 'Value when 'Key : equality> =
        {
            mutable Count: int
            mutable Keys: 'Key[]
            mutable Values: 'Value[]
            mutable HashCodes: int[]
            mutable Nexts: byte[]
            mutable BucketBitShift: int
            mutable WrapAroundMask: int
        }
        static member init () =
            let initialCapacity = 4
            {
                Count = 0
                Keys = Array.zeroCreate 4
                Values = Array.zeroCreate 4
                HashCodes = Array.zeroCreate 4
                Nexts = Array.create 4 Next.empty
                BucketBitShift = 32 - (BitOperations.TrailingZeroCount initialCapacity)
                WrapAroundMask = initialCapacity - 1
            }

open Helpers

type ValueDictionary<'Key, 'Value when 'Key : equality>(entries: seq<'Key * 'Value>) =
    static let initialCapacity = 4
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    let mutable keys : 'Key[] = Array.zeroCreate initialCapacity
    let mutable values : 'Value[] = Array.zeroCreate initialCapacity
    let mutable hashCodes : int[] = Array.zeroCreate initialCapacity
    let mutable nexts : byte[] = Array.create initialCapacity Next.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 32 - (BitOperations.TrailingZeroCount initialCapacity)
    // Used for Wrap Around addition/subtraction of offsets
    let mutable wrapAroundMask = initialCapacity - 1

    let computeBucketIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let isTail (bucketIdx: int) =
        let homeIdx = computeBucketIndex hashCodes[bucketIdx]
        bucketIdx <> homeIdx


    let getParentBucketIdx (hashCode: int) (childBucketIdx: int) =

        let rec loop (ancestorIdx: int) =
            let nextIdx = (ancestorIdx + int nexts[ancestorIdx]) &&& wrapAroundMask
            if nextIdx = childBucketIdx then
                ancestorIdx
            else
                loop nextIdx

        let initialIdx = computeBucketIndex hashCode
        loop initialIdx


    let distanceFromParent (bucketIdx: int) =
        let parentIdx = getParentBucketIdx hashCodes[bucketIdx] bucketIdx
        nexts[parentIdx]


    let setBucket next hashCode key value bucketIdx =
        nexts[bucketIdx] <- next
        hashCodes[bucketIdx] <- hashCode
        keys[bucketIdx] <- key
        values[bucketIdx] <- value


    let removeFromList (bucketIdx: int) =
        let parentBucketIdx = getParentBucketIdx hashCodes[bucketIdx] bucketIdx

        // If this is the Last element in a List, we just need to update the Parent's
        // NextOffset value to 0 and then re-add this entry
        if Next.isLast nexts[bucketIdx] then
            nexts[parentBucketIdx] <- 0uy

        // This element is in the middle of the list so we will remove it from the existing
        // list by having the Parent point to the former Grandchild, now child bucket
        else
            nexts[parentBucketIdx] <- nexts[parentBucketIdx] + nexts[bucketIdx]


    let rec addEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        let rec insertIntoNextEmptyBucket (parentIdx: int) (hashCode: int) (key: 'Key) (value: 'Value) (offset: byte) (bucketIdx: int) =
            if bucketIdx < keys.Length then
                if Next.isAvailable nexts[bucketIdx] then
                    setBucket Next.last hashCode key value bucketIdx
                    count <- count + 1
                    nexts[parentIdx] <- offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif (isTail bucketIdx) && offset > (distanceFromParent bucketIdx) then
                    // Need to take a temporary copy for the purpose of re-insertion
                    let prevHashCode = hashCodes[bucketIdx]
                    let prevKey = keys[bucketIdx]
                    let prevValue = values[bucketIdx]
                    removeFromList bucketIdx
                    setBucket Next.last hashCode key value bucketIdx
                    nexts[parentIdx] <- offset
                    addEntry prevHashCode prevKey prevValue
                else
                    let newOffset = offset + 1uy
                    let newBucketIdx = bucketIdx + 1
                    insertIntoNextEmptyBucket parentIdx hashCode key value newOffset newBucketIdx

            else
                insertIntoNextEmptyBucket parentIdx hashCode key value offset 0


        let rec listSearch (hashCode: int) (key: 'Key) (value: 'Value) (bucketIdx: int) =
            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if hashCodes[bucketIdx] = hashCode &&
               EqualityComparer.Default.Equals (keys[bucketIdx], key) then
                values[bucketIdx] <- value

            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif Next.isLast nexts[bucketIdx] then

                insertIntoNextEmptyBucket bucketIdx hashCode key value 1uy (bucketIdx + 1)

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int nexts[bucketIdx])) &&& wrapAroundMask
                listSearch hashCode key value nextBucketIdx


        let bucketIdx = computeBucketIndex hashCode
        // Check if bucket is Empty or a Tombstone
        if Next.isAvailable nexts[bucketIdx] then
            setBucket Next.last hashCode key value bucketIdx
            count <- count + 1

        // If there is already an entry for this Key, overwrite the Value
        elif hashCodes[bucketIdx] = hashCode &&
            EqualityComparer.Default.Equals (keys[bucketIdx], key) then
            values[bucketIdx] <- value

        // Check if the current Entry is part of a chain for a different
        // BucketIdx and should therefore be evicted
        elif isTail bucketIdx then
            // Move the current entry out of this position
            let prevKey = keys[bucketIdx]
            let prevValue = values[bucketIdx]
            let prevHashCode = hashCodes[bucketIdx]
            removeFromList bucketIdx
            setBucket Next.last hashCode key value bucketIdx
            addEntry prevHashCode prevKey prevValue
            count <- count + 1

        // In this case, the current Entry is the head of a list that
        // we need to append to. We start searching for the tail of the list
        else
            listSearch hashCode key value bucketIdx


    let raiseKeyNotFound hashcode key =
        raise (KeyNotFoundException $"Missing Key: {key} Hashcode: {hashcode}")


    // Increase the size of the backing array if the max fill percent has been reached
    // and migrate all of the entries.
    let resize () =
        // Resize if our fill is >75%
        if count > (keys.Length >>> 2) * 3 then
        // if count > buckets.Length - 2 then
            let prevKeys = keys
            let prevValues = values
            let prevHashCodes = hashCodes
            let prevNexts = nexts

            // Increase the size of the backing store
            let newSize = (prevKeys.Length <<< 1)
            keys <- Array.zeroCreate newSize
            values <- Array.zeroCreate newSize
            hashCodes <- Array.zeroCreate newSize
            nexts <- Array.create newSize Next.empty
            bucketBitShift <- 32 - (BitOperations.TrailingZeroCount newSize)
            wrapAroundMask <- newSize - 1
            count <- 0

            for i in 0..prevKeys.Length - 1 do
                if newSize = 16 && (prevHashCodes[i] = -61060) then
                    ()
                if Next.isEntry prevNexts[i] then
                    addEntry prevHashCodes[i] prevKeys[i] prevValues[i]

    do
        for key, value in entries do
            let hashCode = EqualityComparer.Default.GetHashCode key
            addEntry hashCode key value
            resize()

    new () = ValueDictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) =

            let rec loop (hashCode: int) (key: 'Key) (bucketIdx: int) =

                if hashCode = hashCodes[bucketIdx] &&
                   EqualityComparer.Default.Equals (key, keys[bucketIdx]) then
                    values[bucketIdx]

                elif Next.isLast nexts[bucketIdx] then
                    raiseKeyNotFound hashCode key
                else
                    let nextBucketIdx = (bucketIdx + (int nexts[bucketIdx])) &&& wrapAroundMask
                    loop hashCode key nextBucketIdx

            let hashCode = EqualityComparer.Default.GetHashCode key
            let bucketIdx = computeBucketIndex hashCode

            if hashCode = hashCodes[bucketIdx] &&
               EqualityComparer.Default.Equals (key, keys[bucketIdx]) then
                values[bucketIdx]

            elif Next.isLast nexts[bucketIdx] then
                raiseKeyNotFound hashCode key

            else
                let nextBucketIdx = (bucketIdx + (int nexts[bucketIdx])) &&& wrapAroundMask
                loop hashCode key nextBucketIdx



module StrDictionary =

    module private rec Helpers =

        let strEquals (a: string, b: string) =
            a.AsSpan().SequenceEqual(b.AsSpan())

        let strHashCode (a: string) =
            let mutable hash1 = (5381UL <<< 16) + 5381UL
            let mutable hash2 = hash1
            let mutable length = a.Length

            use ptr = fixed a
            // Move the pointer to where we want to start hashing
            let mutable ptr : nativeptr<UInt64> = retype ptr
            while length > 7 do
                hash1 <- (BitOperations.RotateLeft (hash1, 5) + hash1) ^^^ (NativePtr.get ptr 0)
                hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ (NativePtr.get ptr 1)
                length <- length - 8
                ptr <- NativePtr.add ptr 2

            let mutable ptr : nativeptr<UInt32> = retype ptr
            while length > 3 do
                hash1 <- (BitOperations.RotateLeft (hash1, 5) + hash1) ^^^ uint64 (NativePtr.get ptr 0)
                hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ uint64 (NativePtr.get ptr 1)
                length <- length - 4
                ptr <- NativePtr.add ptr 2

            let mutable ptr : nativeptr<char> = retype ptr
            while length > 0 do
                hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ uint64 (NativePtr.get ptr 1)
                length <- length - 4
                ptr <- NativePtr.add ptr 2


            int (hash1 + (hash2 * 1566083941UL))

        let computeBucketIndex (d: inref<Data<string,_>>) (hashCode: int) =
            let hashProduct = (uint hashCode) * 2654435769u
            int (hashProduct >>> d.BucketBitShift)


        let isTail (d: inref<Data<string,_>>) (bucketIdx: int) =
            let homeIdx = computeBucketIndex &d d.HashCodes[bucketIdx]
            bucketIdx <> homeIdx


        let getParentBucketIdxLoop (d: inref<Data<string, _>>) (childBucketIdx: int) (ancestorIdx: int) =
            let nextIdx = (ancestorIdx + int d.Nexts[ancestorIdx]) &&& d.WrapAroundMask
            if nextIdx = childBucketIdx then
                ancestorIdx
            else
                getParentBucketIdxLoop &d childBucketIdx nextIdx


        let getParentBucketIdx (d: inref<Data<string, _>>) (hashCode: int) (childBucketIdx: int) =
            let initialIdx = computeBucketIndex &d hashCode
            getParentBucketIdxLoop &d childBucketIdx initialIdx


        let distanceFromParent (d: inref<Data<string,_>>) (bucketIdx: int) =
            let parentIdx = getParentBucketIdx &d d.HashCodes[bucketIdx] bucketIdx
            d.Nexts[parentIdx]


        let setBucket (d: inref<Data<string,_>>) next hashCode key value bucketIdx =
            d.Nexts[bucketIdx] <- next
            d.HashCodes[bucketIdx] <- hashCode
            d.Keys[bucketIdx] <- key
            d.Values[bucketIdx] <- value


        let removeFromList (d: inref<Data<string,_>>) (bucketIdx: int) =
            let parentBucketIdx = getParentBucketIdx &d d.HashCodes[bucketIdx] bucketIdx

            // If this is the Last element in a List, we just need to update the Parent's
            // NextOffset value to 0 and then re-add this entry
            if Next.isLast d.Nexts[bucketIdx] then
                d.Nexts[parentBucketIdx] <- 0uy

            // This element is in the middle of the list so we will remove it from the existing
            // list by having the Parent point to the former Grandchild, now child bucket
            else
                d.Nexts[parentBucketIdx] <- d.Nexts[parentBucketIdx] + d.Nexts[bucketIdx]


        let rec insertIntoNextEmptyBucket (d: byref<Data<string, _>>) (parentIdx: int) (hashCode: int) (key: string) (value: 'Value) (offset: byte) (bucketIdx: int) =
            if bucketIdx < d.Keys.Length then
                if Next.isAvailable d.Nexts[bucketIdx] then
                    setBucket &d Next.last hashCode key value bucketIdx
                    d.Count <- d.Count + 1
                    d.Nexts[parentIdx] <- offset
                // Test if this is an entry that is not at its home bucket and is
                // closer to its home than this new entry will be. This is Robin Hood hashing
                elif (isTail &d bucketIdx) && offset > (distanceFromParent &d bucketIdx) then
                    // Need to take a temporary copy for the purpose of re-insertion
                    let prevHashCode = d.HashCodes[bucketIdx]
                    let prevKey = d.Keys[bucketIdx]
                    let prevValue = d.Values[bucketIdx]
                    removeFromList &d bucketIdx
                    setBucket &d Next.last hashCode key value bucketIdx
                    d.Nexts[parentIdx] <- offset
                    addEntry &d prevHashCode prevKey prevValue
                else
                    insertIntoNextEmptyBucket &d parentIdx hashCode key value (offset + 1uy) (bucketIdx + 1)

            else
                insertIntoNextEmptyBucket &d parentIdx hashCode key value offset 0


        let rec listSearch (d: byref<Data<string, _>>) (hashCode: int) (key: string) (value: 'Value) (bucketIdx: int) =
            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if d.HashCodes[bucketIdx] = hashCode &&
               strEquals (d.Keys[bucketIdx], key) then
                d.Values[bucketIdx] <- value

            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif Next.isLast d.Nexts[bucketIdx] then

                insertIntoNextEmptyBucket &d bucketIdx hashCode key value 1uy (bucketIdx + 1)

            // We are not at the end of the list so we compute the next BucketIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextBucketIdx = (bucketIdx + (int d.Nexts[bucketIdx])) &&& d.WrapAroundMask
                listSearch &d hashCode key value nextBucketIdx


        let rec addEntry (d: byref<Data<string,_>>) (hashCode: int) (key: string) (value: 'Value) =

            let bucketIdx = computeBucketIndex &d hashCode
            // Check if bucket is Empty or a Tombstone
            if Next.isAvailable d.Nexts[bucketIdx] then
                setBucket &d Next.last hashCode key value bucketIdx
                d.Count <- d.Count + 1

            // If there is already an entry for this Key, overwrite the Value
            elif d.HashCodes[bucketIdx] = hashCode &&
                strEquals (d.Keys[bucketIdx], key) then
                d.Values[bucketIdx] <- value

            // Check if the current Entry is part of a chain for a different
            // BucketIdx and should therefore be evicted
            elif isTail &d bucketIdx then
                // Move the current entry out of this position
                let prevHashCode = d.HashCodes[bucketIdx]
                let prevKey = d.Keys[bucketIdx]
                let prevValue = d.Values[bucketIdx]
                removeFromList &d bucketIdx
                setBucket &d Next.last hashCode key value bucketIdx
                addEntry &d prevHashCode prevKey prevValue
                d.Count <- d.Count + 1

            // In this case, the current Entry is the head of a list that
            // we need to append to. We start searching for the tail of the list
            else
                listSearch &d hashCode key value bucketIdx


        let resize (d: byref<Data<string, _>>) =
            // Resize if our fill is >75%
            if d.Count > (d.Keys.Length >>> 2) * 3 then
            // if count > buckets.Length - 2 then
                let prevKeys = d.Keys
                let prevValues = d.Values
                let prevHashCodes = d.HashCodes
                let prevNexts = d.Nexts

                // Increase the size of the backing store
                let newSize = (prevKeys.Length <<< 1)
                d.Keys <- Array.zeroCreate newSize
                d.Values <- Array.zeroCreate newSize
                d.HashCodes <- Array.zeroCreate newSize
                d.Nexts <- Array.create newSize Next.empty
                d.BucketBitShift <- 32 - (BitOperations.TrailingZeroCount newSize)
                d.WrapAroundMask <- newSize - 1
                d.Count <- 0

                for i in 0..prevKeys.Length - 1 do
                    if newSize = 16 && (prevHashCodes[i] = -61060) then
                        ()
                    if Next.isEntry prevNexts[i] then
                        addEntry &d prevHashCodes[i] prevKeys[i] prevValues[i]

        let raiseKeyNotFound hashcode key =
            raise (KeyNotFoundException $"Missing Key: {key} Hashcode: {hashcode}")

        let rec searchLoop (d: inref<Data<string, _>>) (hashCode: int) (key: string) (bucketIdx: int) =
            if hashCode = d.HashCodes[bucketIdx] &&
               strEquals (key, d.Keys[bucketIdx]) then
                d.Values[bucketIdx]

            elif Next.isLast d.Nexts[bucketIdx] then
                raiseKeyNotFound hashCode key

            else
                let nextBucketIdx = (bucketIdx + (int d.Nexts[bucketIdx])) &&& d.WrapAroundMask
                searchLoop &d hashCode key nextBucketIdx

        let computeHashingParameters (uniqueKeys: HashSet<string>) =
            let mutable salt = 1
            let mutable searching = true
            let hashCodeAcc = HashSet()

            while searching do
                for key in uniqueKeys do
                    let hashCode =




            ()


    open Helpers


    let create (entries: seq<string * 'Value>) =
        let uniqueKeys = HashSet()
        for key, _ in entries do
            uniqueKeys.Add key |> ignore

        let mutable d : Data<string, 'Value> = Data<_,_>.init()

        for key, value in entries do
            let hashCode = strHashCode minLength key
            addEntry &d hashCode key value
            resize &d

        { new IStaticDictionary<string, 'Value> with
            member _.Item
                with get (key: string) =
                    let hashCode = strHashCode minLength key
                    let bucketIdx = computeBucketIndex &d hashCode

                    if hashCode = d.HashCodes[bucketIdx] &&
                       strEquals (key, d.Keys[bucketIdx]) then
                        d.Values[bucketIdx]

                    elif Next.isLast d.Nexts[bucketIdx] then
                        raiseKeyNotFound hashCode key

                    else
                        let nextBucketIdx = (bucketIdx + (int d.Nexts[bucketIdx])) &&& d.WrapAroundMask
                        searchLoop &d hashCode key nextBucketIdx}
