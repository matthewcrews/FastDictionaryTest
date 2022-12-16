namespace FastDictionaryTest.RobinHood

open System.Collections.Generic

module private Helpers =

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -2
        let tombstone = -1
    
    [<Struct>]
    type Slot<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable Offset : int
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = HashCode.tombstone
        member s.IsEmpty = s.HashCode = HashCode.empty
        member s.IsEntry = s.HashCode >= 0
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0
        
    module Slot =
        
        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Offset = 0
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

open Helpers


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Track the number of items in Dictionary for resize
    let mutable count = 0
    // Create the Buckets with some initial capacity
    let mutable slots : Slot<'Key, 'Value>[] = Array.create 4 Slot.empty
    // BitShift necessary for mapping HashCode to SlotIdx using Fibonacci Hashing
    let mutable slotBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount slots.Length)
    
    // This relies on the number of slots being a power of 2
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF
        
    let computeSlotIndex (hashCode: int) =
        let hashProduct = uint hashCode * 2654435769u
        int (hashProduct >>> slotBitShift)
            
    let rec addEntry (key: 'Key) (value: 'Value) =
        
        let rec loop (offset: int) (hashCode: int) (slotIdx: int) =
            if slotIdx < slots.Length then
                let slot = &slots[slotIdx]
                // Check if slot is Empty or a Tombstone
                if slot.IsAvailable then
                    slot.Offset <- offset
                    slot.HashCode <- hashCode
                    slot.Key <- key
                    slot.Value <- value
                    count <- count + 1
                else
                    // If we reach here, we know the slot is occupied
                    if EqualityComparer.Default.Equals (hashCode, slot.HashCode) &&
                       EqualityComparer.Default.Equals (key, slot.Key) then
                        slot.Value <- value
                        
                    // If this new value is farther from it's Home than the current entry
                    // take the entry for the new value and re-insert the prev entry
                    elif slot.Offset < offset then
                        let prevKey = slot.Key
                        let prevValue = slot.Value
                        slot.Offset <- offset
                        slot.HashCode <- hashCode
                        slot.Key <- key
                        slot.Value <- value
                        count <- count + 1
                        addEntry prevKey prevValue
                    else
                        loop (offset + 1) hashCode (slotIdx + 1)
            else
                // Start over looking from the beginning of the slots
                loop (offset + 1) hashCode 0
                
        let hashCode = computeHashCode key
        let slotIdx = computeSlotIndex hashCode
        loop 0 hashCode slotIdx

    
    let getValue (key: 'Key) =

        let rec loop (hashCode: int) (slotIdx: int) =
            if slotIdx < slots.Length then
                if slots[slotIdx].IsEntry then
                    if EqualityComparer.Default.Equals (hashCode, slots[slotIdx].HashCode) &&
                       EqualityComparer.Default.Equals (key, slots[slotIdx].Key) then
                        slots[slotIdx].Value
                        
                    else
                        loop hashCode (slotIdx + 1)
                        
                elif slots[slotIdx].IsTombstone then
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
            slotBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount slots.Length)
            count <- 0
            
            for slot in oldSlots do
                if slot.IsEntry then
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
