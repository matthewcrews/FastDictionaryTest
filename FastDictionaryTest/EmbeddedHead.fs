namespace FastDictionaryTest.EmbeddedHead

open System.Collections.Generic

module private Helpers =

    [<Struct>]
    type Entry<'Key, 'Value> =
        {
            Key: 'Key
            mutable Value: 'Value
        }

    [<Struct>]
    type Status =
        | Empty
        | Filled

    [<Struct>]
    type Bucket<'Key, 'Value> =
        {
            mutable Status : Status
            mutable Entry : Entry<'Key, 'Value>
            mutable Tail : list<Entry<'Key, 'Value>>
        }

    module Bucket =

        let empty<'Key, 'Value> =
            {
                Status = Empty
                Entry = {
                    Key = Unchecked.defaultof<'Key>
                    Value = Unchecked.defaultof<'Value>
                }
                Tail = []
            }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : Bucket<'Key, 'Value>[] = Array.create 4 Bucket.empty
    // BitShift necessary for mapping HashCode to BucketIdx using Fibonacci Hashing
    let mutable bucketBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)

    // This relies on the size of buckets being a power of 2
    let computeBucketIndex (key: 'Key) =
        let h = EqualityComparer<'Key>.Default.GetHashCode key
        let hashProduct = uint h * 2654435769u
        int (hashProduct >>> bucketBitShift)


    let getIndexForEntry (key: 'Key) (bucket: list<Entry<'Key,'Value>>) =
        let rec loop index key bucket =
            match bucket with
            | [] -> -1
            | head::tail ->
                if EqualityComparer<'Key>.Default.Equals (head.Key, key) then
                    index
                else
                    loop (index + 1) key tail

        loop 0 key bucket


    let addEntryToTail (key: 'Key) (value: 'Value) (bucket: byref<Bucket<'Key,'Value>>) =
        // See if there is already an Entry for the Key in the bucket
        let indexForEntry = getIndexForEntry key bucket.Tail

        // If the index is non-negative, then the Key exists in the bucket
        if indexForEntry >= 0 then
            // In this case, the count of the Dictionary will not increase
            let newEntry = { Key = key; Value = value }
            let newTail = List.updateAt indexForEntry newEntry bucket.Tail
            bucket.Tail <- newTail

        else
            let newEntry = { Key = key; Value = value }
            let newTail = newEntry :: bucket.Tail
            bucket.Tail <- newTail
            count <- count + 1


    let addEntry (key: 'Key) (value: 'Value) =
        let bucketIdx = computeBucketIndex key
        let bucket = &buckets[bucketIdx]

        match bucket.Status with
        | Empty ->
            bucket.Status <- Filled
            bucket.Entry <- { Key = key; Value = value }
            count <- count + 1
        | Filled ->
            let mutable entry = bucket.Entry
            if EqualityComparer<'Key>.Default.Equals (key, entry.Key) then
                entry.Value <- value
            else
                addEntryToTail key value &bucket

    let resize () =
        // Resize if our fill is >75%
        if count > (buckets.Length >>> 2) * 3 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Bucket.empty
            bucketBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount buckets.Length)
            count <- 0

            for bucket in oldBuckets do
                match bucket.Status with
                | Filled ->
                    addEntry bucket.Entry.Key bucket.Entry.Value
                    for entry in bucket.Tail do
                        addEntry entry.Key entry.Value
                | Empty -> ()


    let rec searchTailForKey (key: 'Key) (tail: list<Entry<'Key,'Value>>) =
        match tail with
        | [] ->
            raise (KeyNotFoundException())

        | head::tail ->
            if EqualityComparer<'Key>.Default.Equals (head.Key, key) then
                head.Value
            else
                searchTailForKey key tail


    do
        for k, v in entries do
            addEntry k v
            resize()

    new () = Dictionary<'Key, 'Value>([])

    member d.Item
        with get (key: 'Key) =
            let bucketIdx = computeBucketIndex key
            let bucket = buckets[bucketIdx]

            match bucket.Status with
            | Filled ->
                if EqualityComparer.Default.Equals (key, bucket.Entry.Key) then
                    bucket.Entry.Value
                else
                    searchTailForKey key bucket.Tail

            | Empty ->
                raise (KeyNotFoundException())


        and set (key: 'Key) (value: 'Value) =
                addEntry key value
                resize()
