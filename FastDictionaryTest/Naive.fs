namespace FastDictionaryTest.Naive

open System.Collections.Generic

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
    // We want an AND mask assuming that the size of buckets will always be
    // powers of 2
    let mutable bucketMask = buckets.Length - 1
    
    // This relies on the size of buckets being a power of 2
    let computeBucketIndex (k: 'Key) =
        let h = hash k
        h &&& bucketMask
        
    let getIndexForEntry (key: 'Key) (bucket: list<Entry<_,_>>) =
        let rec loop index key bucket =
            match bucket with
            | [] -> -1
            | head::tail ->
                if head.Key = key then
                    index
                else
                    loop (index + 1) key tail
        
        loop 0 key bucket
        
    let rec getEntryForKey (key: 'Key) (bucket: list<Entry<_,_>>) =
        match bucket with
        | [] ->
            raise (KeyNotFoundException())
        | head::tail ->
            if head.Key = key then
                head
            else
                getEntryForKey key tail
    
    let addEntry (key: 'Key) (value: 'Value) =
        let bucketIdx = computeBucketIndex key
        let bucket = buckets[bucketIdx]
        // See if there is already an Entry for the Key in the bucket
        let indexForEntry = getIndexForEntry key bucket
        
        // If the index is non-negative, then the Key exists in the bucket
        if indexForEntry >= 0 then
            // In this case, the count of the Dictionary will not increase
            let newEntry = { Key = key; Value = value }
            let newBucket = List.updateAt indexForEntry newEntry bucket
            buckets[bucketIdx] <- newBucket
            
        else
            // In this case, the count of the Dictionary will increase and we
            // may need to resize. We add the entry and resize if necessary
            let newEntry = { Key = key; Value = value }
            let newBucket = newEntry :: bucket
            buckets[bucketIdx] <- newBucket
            count <- count + 1
    
    
    let resize () =
        // Only resize when the fill is > 75%
        if count > (buckets.Length >>> 2) * 3 then
            let oldBuckets = buckets
            
            // Increase the size of the backing store
            buckets <- Array.create (buckets.Length <<< 1) []
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
            let bucket = buckets[bucketIdx]
            let entry = getEntryForKey key bucket
            entry.Value
            
        and set (key: 'Key) (value: 'Value) =
                addEntry key value
                resize()
    
    
