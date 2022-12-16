namespace FastDictionaryTest.LinearProbing

open System.Collections.Generic

module private Helpers =

    [<Struct>]
    type Status =
        {
            Value : sbyte
        }
        member s.IsTombstone = s.Value = -1y
        member s.IsEmpty = s.Value = 0y
        member s.IsOccupied = s.Value = 1y
        member s.IsAvailable = s.Value <= 0y
        static member empty = { Value = 0y }
        static member occupied = { Value = 1y }
        static member tombstone = { Value = -1y }
    
    [<Struct>]
    type Slot<'Key, 'Value> =
        {
            mutable Status : Status
            mutable Key : 'Key
            mutable Value : 'Value
        }
        
    module Slot =
        
        let empty<'Key, 'Value> =
            {
                Status = Status.empty
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
    let computeSlotIndex (key: 'Key) =
        let h = EqualityComparer<'Key>.Default.GetHashCode key
        let hashProduct = uint h * 2654435769u
        int (hashProduct >>> slotBitShift)

            
    let addEntry (key: 'Key) (value: 'Value) =
        
        let rec loop (slotIdx: int) =
            if slotIdx < slots.Length then
                let slot = &slots[slotIdx]
                // Check if slot is Empty or a Tombstone
                if slot.Status.IsAvailable then
                    slot.Status <- Status.occupied
                    slot.Key <- key
                    slot.Value <- value
                    count <- count + 1
                else
                    // If we reach here, we know the slot is occupied
                    if EqualityComparer.Default.Equals (key, slot.Key) then
                        slot.Value <- value
                    else
                        loop (slotIdx + 1)
            else
                // Start over looking from the beginning of the slots
                loop 0
                
        let slotIdx = computeSlotIndex key
        loop slotIdx

    let getValue (key: 'Key) =
        
        let rec loop (slotIdx: int) =
            if slotIdx < slots.Length then
                let slot = &slots[slotIdx]
                if slot.Status.IsOccupied then
                    if EqualityComparer.Default.Equals (key, slot.Key) then
                        slot.Value
                    else
                        loop (slotIdx + 1)
                elif slot.Status.IsTombstone then
                    loop (slotIdx + 1)
                else
                    raise (KeyNotFoundException())
            else
                loop 0
        
        let slotIdx = computeSlotIndex key
        loop slotIdx
        
                    
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
                if slot.Status.IsOccupied then
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
    
    
