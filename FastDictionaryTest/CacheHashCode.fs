namespace FastDictionaryTest.CacheHashCode

open System.Collections.Generic

module private Helpers =

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -1
        let tombstone = -2
    
    [<Struct>]
    type Slot<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = -1
        member s.IsEmpty = s.HashCode = -2
        member s.IsOccupied = s.HashCode >= 0
        member s.IsAvailable = s.HashCode < 0
        
    module Slot =
        
        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable slots : Slot<'Key, 'Value>[] = Array.create 4 Slot.empty
    // We want an AND mask assuming that the size of buckets will always be
    // powers of 2
    let mutable slotMask = slots.Length - 1
    
    // This relies on the number of slots being a power of 2
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF
        
    let computeSlotIndex (hashCode: int) =
        hashCode &&& slotMask

            
    let addEntry (key: 'Key) (value: 'Value) =
        
        let rec loop (hashCode: int) (slotIdx: int) =
            if slotIdx < slots.Length then
                let slot = &slots[slotIdx]
                // Check if slot is Empty or a Tombstone
                if slot.IsAvailable then
                    slot.HashCode <- hashCode
                    slot.Key <- key
                    slot.Value <- value
                    count <- count + 1
                else
                    // If we reach here, we know the slot is occupied
                    if EqualityComparer.Default.Equals (hashCode, slot.HashCode) &&
                       EqualityComparer.Default.Equals (key, slot.Key) then
                        slot.Value <- value
                    else
                        loop hashCode (slotIdx + 1)
            else
                // Start over looking from the beginning of the slots
                loop hashCode 0
                
        let hashCode = computeHashCode key
        let slotIdx = computeSlotIndex hashCode
        loop hashCode slotIdx

    
    let getValue (key: 'Key) =
        
        let rec loop (hashCode: int) (slotIdx: int) =
            if slotIdx < slots.Length then
                let slot = &slots[slotIdx]
                if slot.IsOccupied then
                    if EqualityComparer.Default.Equals (hashCode, slot.HashCode) &&
                       EqualityComparer.Default.Equals (key, slot.Key) then
                        slot.Value
                    else
                        loop hashCode (slotIdx + 1)
                elif slot.IsTombstone then
                    loop hashCode (slotIdx + 1)
                else
                    raise (KeyNotFoundException())
            else
                loop hashCode 0
        
        let hashCode = computeHashCode key
        let slotIdx = computeSlotIndex hashCode
        loop hashCode slotIdx
        
                    
    let resize () =
        // Resize if our fill is >75%
        if count > (slots.Length >>> 2) * 3 then
        // if count > slots.Length - 2 then
            let oldSlots = slots
            
            // Increase the size of the backing store
            slots <- Array.create (slots.Length <<< 1) Slot.empty
            slotMask <- slots.Length - 1
            count <- 0
            
            for slot in oldSlots do
                if slot.IsOccupied then
                    addEntry slot.Key slot.Value
        
    do
        for k, v in entries do
            addEntry k v
            resize()

    new () = Dictionary<'Key, 'Value>([])
            
    member d.Item
        with get (key: 'Key) = getValue key
            
        and set (key: 'Key) (value: 'Value) =
                addEntry key value
                resize()
