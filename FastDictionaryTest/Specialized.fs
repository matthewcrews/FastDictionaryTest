namespace FastDictionaryTest.Lambda

open System.Runtime.Intrinsics
open System.Collections.Generic
open System.Runtime.Intrinsics.X86

#nowarn "42"

module Domain =

    type IPriority1 = interface end
    type IPriority2 = interface end

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -1
        let tombstone = -2

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = -1
        member s.IsEmpty = s.HashCode = -2
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0
        member s.IsEntry = s.HashCode >= 0

    module Bucket =

        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

    // This relies on the number of buckets being a power of 2
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF

    let computeBucketIndex (bucketMask: int) (hashCode: int) =
        hashCode &&& bucketMask

    type Internals<'Key, 'Value>() =
        member val Count = 0 with get, set
        member val Buckets = Array.create 4 Bucket.empty<'Key, 'Value> with get, set
        member val BucketMask = 4 - 1 with get, set

    [<RequireQualifiedAccess>]
    module Logic =

        let itemSet
            (key: 'Key)
            (value: 'Value)
            (internals: Internals< 'Key, 'Value>)
            : unit =

            let mutable buckets = internals.Buckets

            let rec loop (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    // Check if bucket is Empty or a Tombstone
                    if buckets[bucketIdx].IsAvailable then
                        buckets[bucketIdx].HashCode <- hashCode
                        buckets[bucketIdx].Key <- key
                        buckets[bucketIdx].Value <- value
                        1
                    else
                        // If we reach here, we know the bucket is occupied
                        if EqualityComparer.Default.Equals (hashCode, buckets[bucketIdx].HashCode) &&
                            EqualityComparer.Default.Equals (key, buckets[bucketIdx].Key) then
                            buckets[bucketIdx].Value <- value
                            0
                        else
                            loop hashCode (bucketIdx + 1)
                else
                    // Start over looking from the beginning of the buckets
                    loop hashCode 0

            let hashCode = computeHashCode key
            let bucketIdx = computeBucketIndex internals.BucketMask hashCode
            internals.Count <- internals.Count + loop hashCode bucketIdx


        let inline itemGetStruct
            (key: 'Key)
            (internals: Internals< 'Key, 'Value>)
            : 'Value =

            let buckets = internals.Buckets
            let hashCode = computeHashCode key

            let rec loop (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    if EqualityComparer.Default.Equals (hashCode, buckets[bucketIdx].HashCode) &&
                        EqualityComparer.Default.Equals (key, buckets[bucketIdx].Key) then
                            buckets[bucketIdx].Value
                    elif buckets[bucketIdx].IsOccupied then
                        loop (bucketIdx + 1)

                    else
                        raise (KeyNotFoundException())

                else
                    loop 0

            let bucketIdx = computeBucketIndex internals.BucketMask hashCode
            loop bucketIdx


        let inline itemGetRef
            (key: 'Key)
            (internals: Internals< 'Key, 'Value>)
            : 'Value =

            let buckets = internals.Buckets

            let rec loop (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length then
                    if buckets[bucketIdx].IsOccupied then
                        if EqualityComparer.Default.Equals (hashCode, buckets[bucketIdx].HashCode) &&
                           EqualityComparer.Default.Equals (key, buckets[bucketIdx].Key) then
                            buckets[bucketIdx].Value

                        else
                            loop hashCode (bucketIdx + 1)

                    elif buckets[bucketIdx].IsTombstone then
                        loop hashCode (bucketIdx + 1)

                    else
                        raise (KeyNotFoundException())
                else
                    loop hashCode 0

            let avxStep (hashCode: int) (bucketIdx: int) =
                if bucketIdx < buckets.Length - 4 then
                    let hashCodeVec = Vector128.Create hashCode
                    let bucketsHashCodeVec = Vector128.Create (
                        buckets[bucketIdx].HashCode,
                        buckets[bucketIdx + 1].HashCode,
                        buckets[bucketIdx + 2].HashCode,
                        buckets[bucketIdx + 3].HashCode
                        )

                    let compareResult =
                        Sse2.CompareEqual (hashCodeVec, bucketsHashCodeVec)
                        |> retype<_, Vector128<float32>>
                    let moveMask = Sse2.MoveMask compareResult
                    let offset = System.Numerics.BitOperations.TrailingZeroCount moveMask

                    if offset <= 3 &&
                       EqualityComparer.Default.Equals (key, buckets[bucketIdx + offset].Key) then
                        buckets[bucketIdx + offset].Value
                    else
                        loop hashCode bucketIdx
                else
                    loop hashCode bucketIdx


            let hashCode = computeHashCode key
            let bucketIdx = computeBucketIndex internals.BucketMask hashCode
            avxStep hashCode bucketIdx


        let resize<'Key, 'Value> (internals: Internals< 'Key, 'Value>) =
            // Resize if our fill is >75%
            if internals.Count > (internals.Buckets.Length >>> 2) * 3 then
            // if count > buckets.Length - 2 then
                let oldBuckets = internals.Buckets

                // Increase the size of the backing store
                internals.Buckets <- Array.create (oldBuckets.Length <<< 1) Bucket.empty
                internals.BucketMask <- internals.Buckets.Length - 1
                internals.Count <- 0

                for bucket in oldBuckets do
                    if bucket.IsEntry then
                        itemSet bucket.Key bucket.Value internals

open Domain


type Dictionary<'Key, 'Value
    when 'Key : equality>
    (internals: Internals<'Key, 'Value>,
     itemGet: 'Key -> Internals<'Key, 'Value> -> 'Value) =

    member _.Internals = internals
    member _.ItemGet = itemGet

    member d.Item
        with inline get (key: 'Key) =
            d.ItemGet key d.Internals


module Dictionary =

    let ofSeq (entries: seq<'Key * 'Value>) =
        let internals = Internals()

        for key, value in entries do
            Logic.itemSet key value internals
            Logic.resize internals
        if typeof<'Key>.IsValueType then
            Dictionary (internals, Logic.itemGetStruct)
        else
            Dictionary (internals, Logic.itemGetRef)
