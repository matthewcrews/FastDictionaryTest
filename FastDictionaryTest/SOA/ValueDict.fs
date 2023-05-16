module FastDictionaryTest.SOA.ValueDict

open System
open System.Collections.Generic
open System.Numerics
open Microsoft.FSharp.NativeInterop
open FastDictionaryTest.SOA.Helpers

#nowarn "9" "42" "51"

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
