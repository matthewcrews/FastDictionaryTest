namespace FastDictionaryTest.Simd2

open System.Runtime.Intrinsics
open System.Collections.Generic
open System.Runtime.Intrinsics.X86

#nowarn "42"

module private Domain =

    let inline retype<'T,'U> (x: 'T) : 'U = (# "" x: 'U #)
    
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
        member s.IsOccupied = s.HashCode >= -1
        member s.IsAvailable = s.HashCode < 0
        
    module Slot =
        
        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

open Domain


type Dictionary<'Key, 'Value when 'Key : equality> (entries: seq<'Key * 'Value>) =
    // Type flag
    let isStruct = typeof<'Key>.IsValueType
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
        
        let hashCode = computeHashCode key
        
        let rec loop (slotIdx: int) =
            if slotIdx < slots.Length then
                if EqualityComparer.Default.Equals (hashCode, slots[slotIdx].HashCode) &&
                   EqualityComparer.Default.Equals (key, slots[slotIdx].Key) then
                       slots[slotIdx].Value
                elif slots[slotIdx].IsOccupied then
                    loop (slotIdx + 1)
                    
                else
                    raise (KeyNotFoundException())

            else
                loop 0
                
        let avxStep (slotIdx: int) =
            if slotIdx < slots.Length - 4 then
                let hashCodeVec = Vector128.Create hashCode
                let slotsHashCodeVec = Vector128.Create (
                    slots[slotIdx].HashCode,
                    slots[slotIdx + 1].HashCode,
                    slots[slotIdx + 2].HashCode,
                    slots[slotIdx + 3].HashCode
                    )
                
                let compareResult =
                    Sse2.CompareEqual (hashCodeVec, slotsHashCodeVec)
                    |> retype<_, Vector128<float32>>
                let moveMask = Sse2.MoveMask compareResult
                let offset = System.Numerics.BitOperations.TrailingZeroCount moveMask
                
                if offset <= 3 &&
                   EqualityComparer.Default.Equals (key, slots[slotIdx + offset].Key) then
                    slots[slotIdx + offset].Value
                else
                    loop slotIdx
            else
                loop slotIdx
            
        
        let slotIdx = computeSlotIndex hashCode
        if isStruct then
            loop slotIdx
        else
            avxStep slotIdx
        
                    
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
