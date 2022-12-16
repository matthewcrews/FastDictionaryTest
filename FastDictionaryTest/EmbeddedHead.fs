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
    type Slot<'Key, 'Value> =
        {
            mutable Status : Status
            mutable Entry : Entry<'Key, 'Value>
            mutable Tail : list<Entry<'Key, 'Value>>
        }
        
    module Slot =
        
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
    let mutable slots : Slot<'Key, 'Value>[] = Array.create 4 Slot.empty
    // BitShift necessary for mapping HashCode to SlotIdx using Fibonacci Hashing
    let mutable slotBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount slots.Length)
    
    // This relies on the size of buckets being a power of 2
    let computeSlotIndex (key: 'Key) =
        let h = EqualityComparer<'Key>.Default.GetHashCode key
        let hashProduct = uint h * 2654435769u
        int (hashProduct >>> slotBitShift)

    
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
    
    
    let addEntryToTail (key: 'Key) (value: 'Value) (slot: byref<Slot<'Key,'Value>>) =
        // See if there is already an Entry for the Key in the bucket
        let indexForEntry = getIndexForEntry key slot.Tail
        
        // If the index is non-negative, then the Key exists in the bucket
        if indexForEntry >= 0 then
            // In this case, the count of the Dictionary will not increase
            let newEntry = { Key = key; Value = value }
            let newTail = List.updateAt indexForEntry newEntry slot.Tail
            slot.Tail <- newTail
            
        else
            let newEntry = { Key = key; Value = value }
            let newTail = newEntry :: slot.Tail
            slot.Tail <- newTail
            count <- count + 1
        
        
    let addEntry (key: 'Key) (value: 'Value) =
        let slotIdx = computeSlotIndex key
        let slot = &slots[slotIdx]
        
        match slot.Status with
        | Empty ->
            slot.Status <- Filled
            slot.Entry <- { Key = key; Value = value }
            count <- count + 1
        | Filled ->
            let mutable entry = slot.Entry
            if EqualityComparer<'Key>.Default.Equals (key, entry.Key) then
                entry.Value <- value
            else
                addEntryToTail key value &slot
                    
    let resize () =
        // Resize if our fill is >75%
        // if count > (slots.Length >>> 2) * 3 then
        if count > slots.Length then
            let oldSlots = slots
            
            // Increase the size of the backing store
            slots <- Array.create (slots.Length <<< 1) Slot.empty
            slotBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount slots.Length)
            count <- 0
            
            for slot in oldSlots do
                match slot.Status with
                | Filled ->
                    addEntry slot.Entry.Key slot.Entry.Value
                    for entry in slot.Tail do
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
            let slotIdx = computeSlotIndex key
            let slot = slots[slotIdx]
            
            match slot.Status with
            | Filled ->
                if EqualityComparer.Default.Equals (key, slot.Entry.Key) then
                    slot.Entry.Value
                else
                    searchTailForKey key slot.Tail
                    
            | Empty ->
                raise (KeyNotFoundException())
            
            
        and set (key: 'Key) (value: 'Value) =
                addEntry key value
                resize()
    
    
