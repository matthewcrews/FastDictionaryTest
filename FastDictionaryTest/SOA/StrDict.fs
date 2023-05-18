module FastDictionaryTest.SOA.StrDict

open System
open System.Collections.Generic
open System.Numerics
open Microsoft.FSharp.NativeInterop
open FastDictionaryTest.SOA.Helpers

#nowarn "9" "42" "51"

module internal rec Helpers =

    [<Struct>]
    type Acc<'Key, 'Value when 'Key : equality> =
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

    [<Struct>]
    type Range =
        {
            Start: int
            Length: int
        }

    [<Struct>]
    type Data<'Value> =
        {
            KeyRanges: Range[]
            KeyChars: char[]
            Values: 'Value[]
            HashCodes: int[]
            Nexts: byte[]
            BucketBitShift: int
            WrapAroundMask: int
        }
        static member ofAcc (acc: Acc<string, 'Value>) =
            let keyChars =
                acc.Keys
                |> Array.collect (fun strKey ->
                    if obj.ReferenceEquals (strKey, null) then
                        [||]
                    else
                        strKey.ToCharArray())

            let mutable i = 0

            let keyRanges =
                acc.Keys
                |> Array.map (fun strKey ->
                    let nextStart = i
                    if obj.ReferenceEquals (strKey, null) then
                        {
                            Start = nextStart
                            Length = 0
                        }
                    else
                        i <- i + strKey.Length
                        {
                            Start = nextStart
                            Length = strKey.Length
                        })

            {
                KeyRanges = keyRanges
                KeyChars = keyChars
                Values = acc.Values
                HashCodes = acc.HashCodes
                Nexts = acc.Nexts
                BucketBitShift = acc.BucketBitShift
                WrapAroundMask = acc.WrapAroundMask
            }


    let strEquals (a: string, b: string) =
        a.AsSpan().SequenceEqual(b.AsSpan())

    let strHashCode (a: string) : int =
        let mutable hash1 = (5381u <<< 16) + 5381u
        let mutable hash2 = hash1
        let mutable length = a.Length

        use ptr = fixed a

        // We are going to index from the end of the string back
        let mutable ptr32 : nativeptr<UInt32> = retype (NativePtr.add ptr (a.Length - 4))
        while length > 3 do
            hash1 <- (BitOperations.RotateLeft (hash1, 5) + hash1) ^^^ uint (NativePtr.get ptr32 0)
            hash2 <- (BitOperations.RotateLeft (hash2, 5) + hash2) ^^^ uint (NativePtr.get ptr32 1)
            length <- length - 4
            ptr32 <- NativePtr.add ptr32 -2

        int (hash1 + (hash2 * 1566083941u))

    let computeBucketIndex bucketBitShift (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let isTail (d: inref<Acc<string,_>>) (bucketIdx: int) =
        let homeIdx = computeBucketIndex d.BucketBitShift d.HashCodes[bucketIdx]
        bucketIdx <> homeIdx


    let getParentBucketIdxLoop (d: inref<Acc<string, _>>) (childBucketIdx: int) (ancestorIdx: int) =
        let nextIdx = (ancestorIdx + int d.Nexts[ancestorIdx]) &&& d.WrapAroundMask
        if nextIdx = childBucketIdx then
            ancestorIdx
        else
            getParentBucketIdxLoop &d childBucketIdx nextIdx


    let getParentBucketIdx (d: inref<Acc<string, _>>) (hashCode: int) (childBucketIdx: int) =
        let initialIdx = computeBucketIndex d.BucketBitShift hashCode
        getParentBucketIdxLoop &d childBucketIdx initialIdx


    let distanceFromParent (d: inref<Acc<string,_>>) (bucketIdx: int) =
        let parentIdx = getParentBucketIdx &d d.HashCodes[bucketIdx] bucketIdx
        d.Nexts[parentIdx]


    let setBucket (d: inref<Acc<string,_>>) next hashCode key value bucketIdx =
        d.Nexts[bucketIdx] <- next
        d.HashCodes[bucketIdx] <- hashCode
        d.Keys[bucketIdx] <- key
        d.Values[bucketIdx] <- value


    let removeFromList (d: inref<Acc<string,_>>) (bucketIdx: int) =
        let parentBucketIdx = getParentBucketIdx &d d.HashCodes[bucketIdx] bucketIdx

        // If this is the Last element in a List, we just need to update the Parent's
        // NextOffset value to 0 and then re-add this entry
        if Next.isLast d.Nexts[bucketIdx] then
            d.Nexts[parentBucketIdx] <- 0uy

        // This element is in the middle of the list so we will remove it from the existing
        // list by having the Parent point to the former Grandchild, now child bucket
        else
            d.Nexts[parentBucketIdx] <- d.Nexts[parentBucketIdx] + d.Nexts[bucketIdx]


    let rec insertIntoNextEmptyBucket (d: byref<Acc<string, _>>) (parentIdx: int) (hashCode: int) (key: string) (value: 'Value) (offset: byte) (bucketIdx: int) =
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


    let rec listSearch (d: byref<Acc<string, _>>) (hashCode: int) (key: string) (value: 'Value) (bucketIdx: int) =
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


    let rec addEntry (d: byref<Acc<string,_>>) (hashCode: int) (key: string) (value: 'Value) =

        let bucketIdx = computeBucketIndex d.BucketBitShift hashCode
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


    let resize (d: byref<Acc<string, _>>) =
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

    let rec searchLoop (d: inref<Data<_>>) (hashCode: int) (key: string) (bucketIdx: int) =
        let strRange = d.KeyRanges[bucketIdx]
        let keyChars = d.KeyChars.AsSpan(strRange.Start, strRange.Length)
        if hashCode = d.HashCodes[bucketIdx] &&
           (key.AsSpan().SequenceEqual(keyChars)) then
            d.Values[bucketIdx]

        elif Next.isLast d.Nexts[bucketIdx] then
            raiseKeyNotFound hashCode key

        else
            let nextBucketIdx = (bucketIdx + (int d.Nexts[bucketIdx])) &&& d.WrapAroundMask
            searchLoop &d hashCode key nextBucketIdx

open Helpers


type StrStaticDict<'Value> internal (d: Data<'Value>) =

    inherit StaticDict<string, 'Value>()

    override _.Item
        with get (key: string) =
            let hashCode = strHashCode key
            let bucketIdx = computeBucketIndex d.BucketBitShift hashCode
            let strRange = d.KeyRanges[bucketIdx]
            let strChars = d.KeyChars.AsSpan(strRange.Start, strRange.Length)

            if hashCode = d.HashCodes[bucketIdx] &&
               (key.AsSpan().SequenceEqual strChars) then
                d.Values[bucketIdx]

            elif Next.isLast d.Nexts[bucketIdx] then
                    raiseKeyNotFound hashCode key

                else
                    let nextBucketIdx = (bucketIdx + (int d.Nexts[bucketIdx])) &&& d.WrapAroundMask
                    searchLoop &d hashCode key nextBucketIdx

let create (entries: seq<string * 'Value>) =
    let uniqueKeys = HashSet()
    for key, _ in entries do
        uniqueKeys.Add key |> ignore

    let mutable acc : Acc<string, 'Value> = Acc<_,_>.init()

    for key, value in entries do
        let hashCode = strHashCode key
        addEntry &acc hashCode key value
        resize &acc

    let d = Data.ofAcc acc

    StrStaticDict d :> StaticDict<string, 'Value>
