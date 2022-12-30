namespace FastDictionaryTest.EmbeddedHead

open System
open System.Net.Cache
open System.Numerics
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

module private Helpers =

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

    [<Literal>]
    let POSITIVE_INT_MASK = 0x7FFF_FFFF

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -1

    type RefEntry<'Key, 'Value>(hashCode: int, key: 'Key, value: 'Value) =
        member val HashCode = hashCode with get
        member val Key = key with get
        member val Value = value with get, set
        [<DefaultValue>] val mutable Tail : RefEntry<'Key, 'Value>


    [<Struct>]
    type StructEntry<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Key : 'Key
            mutable Value : 'Value
            mutable Tail : RefEntry<'Key, 'Value>
        }
        member e.IsEmpty = e.HashCode = HashCode.empty

    module StructEntry =

        let empty<'Key, 'Value> =
            {
                HashCode = HashCode.empty
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
                Tail = Unchecked.defaultof<RefEntry<'Key, 'Value>>
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
    let mutable buckets : StructEntry<'Key, 'Value>[] = Array.create 4 StructEntry.empty
    // BitShift necessary for mapping HashCode to SlotIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (BitOperations.TrailingZeroCount buckets.Length)

    // This relies on the size of buckets being a power of 2
    let computeBucketIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let addEntry (hashCode: int) (key: 'Key) (value: 'Value) =

        if typeof<'Key>.IsValueType then

            let rec refLoop (rEntry: RefEntry<_,_>) =
                if rEntry.HashCode = hashCode &&
                   EqualityComparer.Default.Equals (rEntry.Key, key) then

                    rEntry.Value <- value

                elif obj.ReferenceEquals (rEntry.Tail, null) then
                    rEntry.Tail <- RefEntry (hashCode, key, value)
                    count <- count + 1

                else
                    refLoop rEntry.Tail

            let bucketIdx = computeBucketIndex hashCode
            let sEntry = &buckets[bucketIdx]

            if sEntry.HashCode = hashCode &&
               EqualityComparer.Default.Equals (sEntry.Key, key) then

                sEntry.Value <- value

            elif obj.ReferenceEquals (sEntry.Tail, null) then
                sEntry.Tail <- RefEntry (hashCode, key, value)
                count <- count + 1
            else
                refLoop sEntry.Tail

        else

            let rec refLoop (rEntry: RefEntry<_,_>) =
                if rEntry.HashCode = hashCode &&
                   refComparer.Equals (rEntry.Key, key) then

                    rEntry.Value <- value

                elif obj.ReferenceEquals (rEntry.Tail, null) then
                    rEntry.Tail <- RefEntry (hashCode, key, value)
                    count <- count + 1

                else
                    refLoop rEntry.Tail

            let bucketIdx = computeBucketIndex hashCode
            let sEntry = &buckets[bucketIdx]

            if sEntry.HashCode = hashCode &&
               refComparer.Equals (sEntry.Key, key) then

                sEntry.Value <- value

            elif obj.ReferenceEquals (sEntry.Tail, null) then
                sEntry.Tail <- RefEntry (hashCode, key, value)
                count <- count + 1

            else
                refLoop sEntry.Tail


    let getStructValue (key: 'Key) =
        let hashCode = EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK

        let rec loop (rEntry: RefEntry<'Key, 'Value>) =
            if rEntry.HashCode = hashCode &&
               EqualityComparer.Default.Equals (rEntry.Key, key) then
                rEntry.Value

            else
                if obj.ReferenceEquals (rEntry.Tail, null) then
                    raise (KeyNotFoundException())
                else
                    loop rEntry.Tail

        let bucketIdx = computeBucketIndex hashCode
        let sEntry = buckets[bucketIdx]

        if sEntry.HashCode = hashCode &&
           EqualityComparer.Default.Equals (sEntry.Key, key) then
            sEntry.Value

        elif (obj.ReferenceEquals (sEntry.Tail, null)) then
            raise (KeyNotFoundException())

        else
            loop sEntry.Tail


    let getRefValue (key: 'Key) =

        let hashCode = (refComparer.GetHashCode key) &&& POSITIVE_INT_MASK

        let rec refLoop (rEntry: RefEntry<'Key, 'Value>) =
            if rEntry.HashCode = hashCode &&
               refComparer.Equals (rEntry.Key, key) then
                rEntry.Value
            else
                if obj.ReferenceEquals (rEntry.Tail, null) then
                    raise (KeyNotFoundException())
                else
                    refLoop rEntry.Tail

        let bucketIdx = computeBucketIndex hashCode
        let sEntry = buckets[bucketIdx]

        if sEntry.HashCode = hashCode &&
           refComparer.Equals (sEntry.Key, key) then
            sEntry.Value

        elif (obj.ReferenceEquals (sEntry.Tail, null)) then
            raise (KeyNotFoundException())

        else
            refLoop sEntry.Tail


    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) StructEntry.empty
            bucketBitShift <- 64 - (BitOperations.TrailingZeroCount buckets.Length)
            count <- 0

            let rec refLoop (rEntry: RefEntry<'Key, 'Value>) =
                addEntry rEntry.HashCode rEntry.Key rEntry.Value
                if not (obj.ReferenceEquals (rEntry.Tail, null)) then
                    refLoop rEntry.Tail

            for sEntry in oldBuckets do
                addEntry sEntry.HashCode sEntry.Key sEntry.Value

                if not (obj.ReferenceEquals (sEntry.Tail, null)) then
                    refLoop sEntry.Tail


    do
        for key, value in entries do
            let hashCode =
                if typeof<'Key>.IsValueType then
                    EqualityComparer.Default.GetHashCode key &&& POSITIVE_INT_MASK
                else
                    refComparer.GetHashCode key &&& POSITIVE_INT_MASK

            addEntry hashCode key value
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) =
            if typeof<'Key>.IsValueType then
                getStructValue key
            else
                getRefValue key
