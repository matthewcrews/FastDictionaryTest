namespace FastDictionaryTest.Monomorphization

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

    [<Struct>]
    type Internals<'Key, 'Value> =
        {
            mutable Count : int
            mutable Buckets : Bucket<'Key, 'Value>[]
            mutable BucketBitShift : int
        }

    module Internals =

        let empty<'Key, 'Value> =
            let initialCapacity = 4
            {
                Count = 0
                Buckets = Array.create initialCapacity Bucket.empty<'Key, 'Value>
                BucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount initialCapacity)
            }

    [<RequireQualifiedAccess>]
    type Logic =

        static member AddEntry< 'Key, 'Value> (key: 'Key, value: 'Value, internals: byref<Internals< 'Key, 'Value>>) =
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
            let bucketIdx = computeBucketIndex internals.BucketBitShift hashCode
            internals.Count <- internals.Count + loop hashCode bucketIdx


        static member GetValue<'Key, 'Value when 'Key: struct> (key: 'Key, internals: inref<Internals< 'Key, 'Value>>, ?priority: IPriority1) : 'Value =
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

            let bucketIdx = computeBucketIndex internals.BucketBitShift hashCode
            loop bucketIdx


        static member GetValue<'Key, 'Value when 'Key: not struct>(key: 'Key, internals: inref<Internals< 'Key, 'Value>>, ?priority: IPriority2) : 'Value =

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

            let avxStep (bucketIdx: int) =
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
                        loop bucketIdx
                else
                    loop bucketIdx

            let bucketIdx = computeBucketIndex internals.BucketBitShift hashCode
            avxStep bucketIdx


        static member Resize<'Key, 'Value> (internals: byref<Internals< 'Key, 'Value>>) =
            // Resize if our fill is >75%
            if internals.Count > (internals.Buckets.Length >>> 2) * 3 then
            // if count > buckets.Length - 2 then
                let oldBuckets = internals.Buckets

                // Increase the size of the backing store
                internals.Buckets <- Array.create (oldBuckets.Length <<< 1) Bucket.empty
                internals.BucketBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount internals.Buckets.Length)
                internals.Count <- 0

                for bucket in oldBuckets do
                    if bucket.IsEntry then
                        Logic.AddEntry (bucket.Key, bucket.Value, &internals)

open Domain

type Dictionary<'KeyS, 'KeyR, 'Value
    when 'KeyS : equality
    and 'KeyS: struct
    and 'KeyR : equality
    and 'KeyR: not struct>
    private (internalsS: Internals<'KeyS, 'Value>, internalsR: Internals<'KeyR, 'Value>) =


    static member ofSeq (entries: seq<'KeyS * 'Value>) =
        let mutable internalsS : Internals<'KeyS, 'Value> = Internals.empty
        let mutable internalsR : Internals<'KeyR, 'Value> = Unchecked.defaultof<_>
        do
            for k, v in entries do
                Logic.AddEntry (k, v, &internalsS)
                Logic.Resize &internalsS

        Dictionary<'KeyS, 'KeyR, 'Value>(internalsS, internalsR)

    static member ofSeq (entries: seq<'KeyR * 'Value>) =
        let mutable internalsS : Internals<'KeyS, 'Value> = Unchecked.defaultof<_>
        let mutable internalsR : Internals<'KeyR, 'Value> = Internals.empty
        do
            for k, v in entries do
                Logic.AddEntry (k, v, &internalsR)
                Logic.Resize &internalsR

        Dictionary<'KeyS, 'KeyR, 'Value>(internalsS, internalsR)


    member d.Item
        with get (key: 'KeyS) =
            Logic.GetValue (key, &internalsS)

        and set (key: 'KeyS) (value: 'Value) =
                let mutable internals = internalsS
                Logic.AddEntry (key, value, &internals)
                Logic.Resize &internals

    member d.Item
        with get (key: 'KeyR) =
            Logic.GetValue (key, &internalsR)
