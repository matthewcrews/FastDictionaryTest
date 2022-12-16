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
    
    // This relies on the number of slots being a power of 2
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF
        
    let computeSlotIndex (hashCode: int) =
        let hashProduct = (uint hashCode) * 2654435769u
        int (hashProduct >>> slotBitShift)
            
    let rec addEntry (key: 'Key) (value: 'Value) =
        let hashCode = computeHashCode key
        
        let rec emptySlotSearch (offset: int) (slotIdx: int) : int =
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
                    emptySlotSearch (offset + 1) (slotIdx + 1)
                
            else
                emptySlotSearch offset 0
        
        
        let rec listSearch (slotIdx: int) =
            if slots[slotIdx].IsLast then
                slots[slotIdx].NextOffset <- byte (emptySlotSearch 1 (slotIdx + 1))
                
            else
                // Compute the next index which takes the wrap around logic into account
                let nextSlotIdx = (slotIdx + (int slots[slotIdx].NextOffset)) &&& wrapAroundMask
                listSearch nextSlotIdx

                        
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
        else
            listSearch slotIdx

    
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
