[<RequireQualifiedAccess>]
module FastDictionaryTest.Naive

module private Helpers =

    type Entry<'Key, 'Value> =
        {
            Key: 'Key
            Value: 'Value
        }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable buckets : list<Entry<'Key, 'Value>>[] = Array.create 4 []
    // Map a 'Key to the bucket we expect it to be in
    let computeBucketIndex (key: 'Key) =
        let h = (hash key) &&& 0x7FFF_FFFF
        let bucketIdx = h % buckets.Length
        bucketIdx


    let addEntry (key: 'Key) (value: 'Value) =

        let rec loop (acc: Entry<_,_> list) (remaining: Entry<_,_> list) =
            match remaining with
            | [] ->
                let newEntry = { Key = key; Value = value }
                count <- count + 1
                newEntry :: acc
            | head::tail ->
                if head.Key = key then
                    let updatedEntry = { head with Value = value }
                    (updatedEntry::acc) @ tail
                else
                    loop (head::acc) tail

        let bucketIdx = computeBucketIndex key
        let bucket = buckets[bucketIdx]
        let updatedBucket = loop [] bucket
        buckets[bucketIdx] <- updatedBucket


    let getValue (key: 'Key) =
        let bucketIdx = computeBucketIndex key
        buckets[bucketIdx]
        |> List.find (fun entry -> entry.Key = key)
        |> fun entry -> entry.Value


    let resize () =
        // Only resize when the fill is > 75%
        if count > (buckets.Length >>> 2) * 3 then
            let oldBuckets = buckets

            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) []
            count <- 0

            for bucket in oldBuckets do
                for entry in bucket do
                    addEntry entry.Key entry.Value

    do
        for key, value in entries do
            addEntry key value
            resize()

    new () = Dictionary([])

    member d.Item
        with get (key: 'Key) = getValue key
