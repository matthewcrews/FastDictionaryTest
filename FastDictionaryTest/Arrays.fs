namespace FastDictionaryTest.Arrays

open System.Collections.Generic

module private Helpers =

    [<Struct>]
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
    let mutable buckets : Entry<'Key, 'Value>[][] = Array.create 4 Array.empty
    // We want an AND mask assuming that the size of buckets will always be
    // powers of 2
    let mutable bucketMask = buckets.Length - 1
    
    // This relies on the size of buckets being a power of 2
    let computeBucketIndex (key: 'Key) =
        let h = EqualityComparer<'Key>.Default.GetHashCode key
        h &&& bucketMask
        
    let getIndexForEntry (key: 'Key) (bucket: Entry<_,_>[]) =
        let mutable result = -1
        
        bucket
        |> Array.iteri (fun i entry ->
            if EqualityComparer<'Key>.Default.Equals (entry.Key, key) then
                result <- i)

        result
            
    let getValueForKey (key: 'Key) (bucket: Entry<_,_>[]) =
        
        // Talk about this versus Array.find
        let rec loop i =
            if i >= bucket.Length then
                raise (KeyNotFoundException())
            elif EqualityComparer<'Key>.Default.Equals (bucket[i].Key, key) then
                bucket[i].Value
            else
                loop (i + 1)

        loop 0
    
        
    let addEntry (key: 'Key) (value: 'Value) =
        let bucketIdx = computeBucketIndex key
        let bucket = buckets[bucketIdx]
        // See if there is already an Entry for the Key in the bucket
        let indexForEntry = getIndexForEntry key bucket
        
        // If the index is non-negative, then the Key exists in the bucket
        if indexForEntry >= 0 then
            // In this case, the count of the Dictionary will not increase
            let newEntry = { Key = key; Value = value }
            buckets[bucketIdx][indexForEntry] <- newEntry
            
        else
            // In this case, the count of the Dictionary will increase and we
            // may need to resize. We add the entry and resize if necessary
            let newEntry = { Key = key; Value = value }
            let newBucket = Array.append [|newEntry|] bucket
            buckets[bucketIdx] <- newBucket
            count <- count + 1
    
    
    let resize () =
        // Only resize when the fill is > 75%
        if count > (buckets.Length >>> 2) * 3 then
            let oldBuckets = buckets
            
            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) Array.empty
            bucketMask <- buckets.Length - 1
            count <- 0
            
            for bucket in oldBuckets do
                for entry in bucket do
                    addEntry entry.Key entry.Value
        
    do
        for k, v in entries do
            addEntry k v

    new () = Dictionary([])
            
    member d.Item
        with get (key: 'Key) =
            let bucketIdx = computeBucketIndex key
            getValueForKey key buckets[bucketIdx]
            
        and set (key: 'Key) (value: 'Value) =
                addEntry key value
                resize()
    
    
