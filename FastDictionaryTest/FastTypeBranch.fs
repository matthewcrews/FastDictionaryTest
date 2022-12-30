namespace FastDictionaryTest.FastTypeBranch

open System
open System.Numerics
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9" "42" "51"

module private Helpers =

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)

    type Entry<'Key, 'Value> =
        {
            HashCode: int
            Key: 'Key
            Value: 'Value
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
    let mutable buckets : list<Entry<'Key, 'Value>>[] = Array.create 4 []
    // BitShift necessary for mapping HashCode to SlotIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)

    // This relies on the size of buckets being a power of 2
    let computeBucketIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let addEntry (key: 'Key) (value: 'Value) =

        if typeof<'Key>.IsValueType then
            let hashCode = EqualityComparer.Default.GetHashCode key

            let rec loop (acc: Entry<_,_> list) (remaining: Entry<_,_> list) =
                match remaining with
                | [] ->
                    let newEntry = { HashCode = hashCode; Key = key; Value = value }
                    // Increment the count since we have added an entry
                    count <- count + 1
                    newEntry :: acc

                | head::tail ->
                    if head.HashCode = hashCode && EqualityComparer<'Key>.Default.Equals (head.Key, key) then
                        // Do not increment the count in this case because we are just overwriting an
                        // exising value
                        let updatedEntry = { head with Value = value }
                        (updatedEntry::acc) @ tail
                    else
                        loop (head::acc) tail

            let bucketIdx = computeBucketIndex hashCode
            let bucket = buckets[bucketIdx]
            let updatedBucket = loop [] bucket
            buckets[bucketIdx] <- updatedBucket

        else
            let hashCode = refComparer.GetHashCode key

            let rec loop (acc: Entry<_,_> list) (remaining: Entry<_,_> list) =
                match remaining with
                | [] ->
                    let newEntry = { HashCode = hashCode; Key = key; Value = value }
                    // Increment the count since we have added an entry
                    count <- count + 1
                    newEntry :: acc

                | head::tail ->
                    if head.HashCode = hashCode && refComparer.Equals (head.Key, key) then
                        // Do not increment the count in this case because we are just overwriting an
                        // exising value
                        let updatedEntry = { head with Value = value }
                        (updatedEntry::acc) @ tail
                    else
                        loop (head::acc) tail

            let bucketIdx = computeBucketIndex hashCode
            let bucket = buckets[bucketIdx]
            let updatedBucket = loop [] bucket
            buckets[bucketIdx] <- updatedBucket


    let getStructValue (key: 'Key) =
        let hashCode = EqualityComparer.Default.GetHashCode key

        let rec loop (entry: Entry<_,_> list) =
            match entry with
            | [] ->
                raise (KeyNotFoundException())
            | head::tail ->
                if head.HashCode = hashCode && EqualityComparer.Default.Equals (head.Key, key) then
                    head.Value
                else
                    loop tail

        let bucketIdx = computeBucketIndex hashCode
        let bucket = buckets[bucketIdx]
        loop bucket


    let getRefValue (key: 'Key) =
        let hashCode = refComparer.GetHashCode key

        let rec loop (entry: Entry<_,_> list) =
            match entry with
            | [] ->
                raise (KeyNotFoundException())
            | head::tail ->
                if head.HashCode = hashCode && refComparer.Equals (head.Key, key) then
                    head.Value
                else
                    loop tail

        let bucketIdx = computeBucketIndex hashCode
        let bucket = buckets[bucketIdx]
        loop bucket


    let resize () =
        // Only resize when the fill is > 75%
        if count > (buckets.Length >>> 2) * 3 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) []
            bucketBitShift <- 64 - (BitOperations.TrailingZeroCount buckets.Length)
            count <- 0

            for bucket in oldBuckets do
                for entry in bucket do
                    addEntry entry.Key entry.Value

    do
        for k, v in entries do
            addEntry k v
            resize()

    new () = Dictionary([])

    member d.Item
        with get (key: 'Key) =
            if typeof<'Key>.IsValueType then
                getStructValue key
            else
                getRefValue key
