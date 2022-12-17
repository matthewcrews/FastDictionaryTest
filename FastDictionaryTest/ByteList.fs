namespace FastDictionaryTest.ByteList

open System.Collections.Generic

module private Helpers =

    [<RequireQualifiedAccess>]
    module HashCode =
        let empty = -2
        let tombstone = -1
    
    [<RequireQualifiedAccess>]
    module Offset =
        let head = 0uy
        let last = 0uy
    
    [<Struct>]
    type Slot<'Key, 'Value> =
        {
            mutable HashCode : int
            mutable PrevOffset : byte
            mutable NextOffset : byte
            mutable Key : 'Key
            mutable Value : 'Value
        }
        member s.IsTombstone = s.HashCode = HashCode.tombstone
        member s.IsEmpty = s.HashCode = HashCode.empty
        member s.IsEntry = s.HashCode >= 0
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0
        member s.IsHead = s.PrevOffset = Offset.head
        member s.IsLast = s.NextOffset = Offset.last
        member s.IsTail = s.PrevOffset > 0uy
        
    module Slot =
        
        let empty<'Key, 'Value> =
            {
                HashCode = -1
                PrevOffset = 0uy
                NextOffset = 0uy
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
    // Used for Wrap Around addition of offsets
    let mutable wrapAroundMask = slots.Length - 1
    
    // This relies on the number of slots being a power of 2. We also make sure the HashCode is positive
    // since we use the top bit to indicate whether the slot is available. It could either be empty
    // or a tombstone.
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF


    let computeSlotIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> slotBitShift)


    let rec addEntry (key: 'Key) (value: 'Value) =
        
        let rec insertIntoNextEmptySlot (hashCode: int) (offset: int) (slotIdx: int) : int =
            if slotIdx < slots.Length then
                let slot = &slots[slotIdx]
                if slot.IsAvailable then
                    slot.PrevOffset <- byte offset
                    slot.NextOffset <- 0uy
                    slot.HashCode <- hashCode
                    slot.Key <- key
                    slot.Value <- value
                    count <- count + 1
                    offset
                else
                    insertIntoNextEmptySlot hashCode (offset + 1) (slotIdx + 1)
                
            else
                insertIntoNextEmptySlot hashCode offset 0
        
        let evict (slotIdx: int) =
            let slot = &slots[slotIdx]
            let parentSlotIdx = (slotIdx - (int slot.PrevOffset)) &&& wrapAroundMask
            
            // If this is the Last element in a List, we just need to update the Parent's
            // NextOffset value to 0 and then re-add this entry
            if slot.IsLast then
                slots[parentSlotIdx].NextOffset <- 0uy
                addEntry slot.Key slot.Value
            
            // This element is in the middle of the list so we will remove it from the existing
            // list and then re-add it
            else
                let childSlotIdx = (slotIdx + (int slot.NextOffset)) &&& wrapAroundMask
                slots[parentSlotIdx].NextOffset <- slots[parentSlotIdx].NextOffset + slot.NextOffset
                slots[childSlotIdx].PrevOffset <- slots[childSlotIdx].PrevOffset + slot.PrevOffset
                addEntry slot.Key slot.Value
        
        let rec listSearch (hashCode: int) (slotIdx: int) =
            let slot = &slots[slotIdx]
            
            // Check if we have found an existing Entry for the Key
            // If we have, we want to update the value
            if slot.HashCode = hashCode && slot.Key = key then
                slot.Value <- value
                
            // The Entry is not a match for Key so we need to check if we have come
            // to the end of the list. If we have, then we search for empty space
            // to add our new Key/Value and update the offset for the previous Last entry.
            elif slots[slotIdx].IsLast then
                
                slots[slotIdx].NextOffset <- byte (insertIntoNextEmptySlot hashCode 1 (slotIdx + 1))
                
            // We are not at the end of the list so we compute the next SlotIdx and move
            // to the next Entry. We use a wrap around mask to ensure we don't go outside
            // the bounds of the array.
            else
                // Compute the next index which takes the wrap around logic into account
                let nextSlotIdx = (slotIdx + (int slots[slotIdx].NextOffset)) &&& wrapAroundMask
                listSearch hashCode nextSlotIdx


        let hashCode = computeHashCode key
        let slotIdx = computeSlotIndex hashCode
        let slot = &slots[slotIdx]
        // Check if slot is Empty or a Tombstone
        if slot.IsAvailable then
            slot.PrevOffset <- Offset.head
            slot.NextOffset <- Offset.last
            slot.HashCode <- hashCode
            slot.Key <- key
            slot.Value <- value
            count <- count + 1
            
        // If there is already an entry for this Key, overwrite the Value
        elif slot.HashCode = hashCode && slot.Key = key then
            slot.Value <- value
            
        // Check if the current Entry is part of a chain for a different
        // SlotIdx and should therefore be evicted
        elif slot.IsTail then
            // Move the current entry out of this position
            evict slotIdx
            slot.PrevOffset <- Offset.head
            slot.NextOffset <- Offset.last
            slot.HashCode <- hashCode
            slot.Key <- key
            slot.Value <- value
            count <- count + 1
            
        // In this case, the current Entry is the head of a list that
        // we need to append to. We start searching for the tail of the list
        else
            listSearch hashCode slotIdx

    
    let getValue (key: 'Key) =
        let hashCode = computeHashCode key

        let rec loop (slotIdx: int) =
            if EqualityComparer.Default.Equals (hashCode, slots[slotIdx].HashCode) &&
               EqualityComparer.Default.Equals (key, slots[slotIdx].Key) then
                slots[slotIdx].Value
            elif slots[slotIdx].IsLast then
                raise (KeyNotFoundException())
            else
                let nextSlotIdx = (slotIdx + (int slots[slotIdx].NextOffset)) &&& wrapAroundMask
                loop nextSlotIdx
    
        let slotIdx = computeSlotIndex hashCode
        loop slotIdx
        

    // Increase the size of the backing array if the max fill percent has been reached
    // and migrate all of the entries.
    let resize () =
        // Resize if our fill is >75%
        if count > (slots.Length >>> 2) * 3 then
        // if count > slots.Length - 2 then
            let oldSlots = slots
            
            // Increase the size of the backing store
            slots <- Array.create (slots.Length <<< 1) Slot.empty
            slotBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount slots.Length)
            wrapAroundMask <- slots.Length - 1
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
