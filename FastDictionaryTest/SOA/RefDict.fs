module FastDictionaryTest.SOA.RefDict

open System
open System.Collections.Generic
open System.Numerics
open FastDictionaryTest.SOA.Helpers

#nowarn "9" "42" "51"

module internal rec Helpers =

    [<Struct>]
    type Acc<'Key, 'Value when 'Key : equality> =
        {
            Comparer: EqualityComparer<'Key>
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
                Comparer = EqualityComparer.Default
                Count = 0
                Keys = Array.zeroCreate 4
                Values = Array.zeroCreate 4
                HashCodes = Array.zeroCreate 4
                Nexts = Array.create 4 Next.empty
                BucketBitShift = 32 - (BitOperations.TrailingZeroCount initialCapacity)
                WrapAroundMask = initialCapacity - 1
            }

    [<Struct>]
    type Data<'Key, 'Value when 'Key : equality> =
        {
            Comparer: EqualityComparer<'Key>
            Keys: 'Key[]
            Values: 'Value[]
            HashCodes: int[]
            Nexts: byte[]
            BucketBitShift: int
            WrapAroundMask: int
        }
        static member ofAcc (acc: Acc<'Key, 'Value>) =
            {
                Comparer = acc.Comparer
                Keys = acc.Keys
                Values = acc.Values
                HashCodes = acc.HashCodes
                Nexts = acc.Nexts
                BucketBitShift = acc.BucketBitShift
                WrapAroundMask = acc.WrapAroundMask
            }

    let computeBucketIndex bucketBitShift (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let isTail (d: inref<Acc<'Key,_>>) (bucketIdx: int) =
        let homeIdx = computeBucketIndex d.BucketBitShift d.HashCodes[bucketIdx]
        bucketIdx <> homeIdx


    let getParentBucketIdxLoop (d: inref<Acc<'Key, _>>) (childBucketIdx: int) (ancestorIdx: int) =
        let nextIdx = (ancestorIdx + int d.Nexts[ancestorIdx]) &&& d.WrapAroundMask
        if nextIdx = childBucketIdx then
            ancestorIdx
        else
            getParentBucketIdxLoop &d childBucketIdx nextIdx


    let getParentBucketIdx (d: inref<Acc<'Key, _>>) (hashCode: int) (childBucketIdx: int) =
        let initialIdx = computeBucketIndex d.BucketBitShift hashCode
        getParentBucketIdxLoop &d childBucketIdx initialIdx


    let distanceFromParent (d: inref<Acc<'Key,_>>) (bucketIdx: int) =
        let parentIdx = getParentBucketIdx &d d.HashCodes[bucketIdx] bucketIdx
        d.Nexts[parentIdx]


    let setBucket (d: inref<Acc<_,_>>) next hashCode key value bucketIdx =
        d.Nexts[bucketIdx] <- next
        d.HashCodes[bucketIdx] <- hashCode
        d.Keys[bucketIdx] <- key
        d.Values[bucketIdx] <- value


    let removeFromList (d: inref<Acc<'Key,_>>) (bucketIdx: int) =
        let parentBucketIdx = getParentBucketIdx &d d.HashCodes[bucketIdx] bucketIdx

        // If this is the Last element in a List, we just need to update the Parent's
        // NextOffset value to 0 and then re-add this entry
        if Next.isLast d.Nexts[bucketIdx] then
            d.Nexts[parentBucketIdx] <- 0uy

        // This element is in the middle of the list so we will remove it from the existing
        // list by having the Parent point to the former Grandchild, now child bucket
        else
            d.Nexts[parentBucketIdx] <- d.Nexts[parentBucketIdx] + d.Nexts[bucketIdx]


    let rec insertIntoNextEmptyBucket (d: byref<Acc<'Key, _>>) (parentIdx: int) (hashCode: int) (key: 'Key) (value: 'Value) (offset: byte) (bucketIdx: int) =
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


    let rec listSearch (d: byref<Acc<_, _>>) (hashCode: int) (key: 'Key) (value: 'Value) (bucketIdx: int) =
        // Check if we have found an existing Entry for the Key
        // If we have, we want to update the value
        if d.HashCodes[bucketIdx] = hashCode &&
           d.Comparer.Equals (key, d.Keys[bucketIdx]) then
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


    let rec addEntry (d: byref<Acc<_,_>>) (hashCode: int) (key: 'Key) (value: 'Value) =

        let bucketIdx = computeBucketIndex d.BucketBitShift hashCode
        // Check if bucket is Empty or a Tombstone
        if Next.isAvailable d.Nexts[bucketIdx] then
            setBucket &d Next.last hashCode key value bucketIdx
            d.Count <- d.Count + 1

        // If there is already an entry for this Key, overwrite the Value
        elif d.HashCodes[bucketIdx] = hashCode &&
            d.Comparer.Equals (key, d.Keys[bucketIdx]) then
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


    let resize (d: byref<Acc<'Key, _>>) =
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
                if Next.isEntry prevNexts[i] then
                    addEntry &d prevHashCodes[i] prevKeys[i] prevValues[i]

    let raiseKeyNotFound hashcode key =
        raise (KeyNotFoundException $"Missing Key: {key} Hashcode: {hashcode}")


open Helpers


type RefStaticDict<'Key, 'Value when 'Key : equality> internal (d: Data<'Key, 'Value>) =

    inherit StaticDict<'Key, 'Value>()

    override _.Item
        with get (key: 'Key) =
            let hashCode = d.Comparer.GetHashCode key
            let mutable bucketIdx = computeBucketIndex d.BucketBitShift hashCode
            let mutable searching = true
            let mutable result = Unchecked.defaultof<'Value>

            while searching do
                result <- d.Values[bucketIdx]

                if hashCode = d.HashCodes[bucketIdx] && d.Comparer.Equals (key, d.Keys[bucketIdx])  then
                    searching <- false

                elif Next.isLast d.Nexts[bucketIdx] then
                    raiseKeyNotFound hashCode key

                else
                    bucketIdx <- (bucketIdx + (int d.Nexts[bucketIdx])) &&& d.WrapAroundMask

            result


let create (entries: seq<'Key * 'Value>) =
    let uniqueKeys = HashSet()
    for key, _ in entries do
        uniqueKeys.Add key |> ignore

    let mutable acc : Acc<'Key, 'Value> = Acc<_,_>.init()

    for key, value in entries do
        let hashCode = acc.Comparer.GetHashCode key
        addEntry &acc hashCode key value
        resize &acc

    let d = Data.ofAcc acc
    RefStaticDict d :> StaticDict<'Key, 'Value>
