namespace FastDictionaryTest.Lambda

open System.Runtime.Intrinsics
open System.Collections.Generic
open System.Runtime.Intrinsics.X86

#nowarn "42"

module Domain =

    type IPriority1 = interface end
    type IPriority2 = interface end
    
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
        member s.IsEntry = s.HashCode >= 0
        
    module Slot =
        
        let empty<'Key, 'Value> =
            {
                HashCode = -1
                Key = Unchecked.defaultof<'Key>
                Value = Unchecked.defaultof<'Value>
            }

    // This relies on the number of slots being a power of 2
    let computeHashCode (key: 'Key) =
        // Ensure the HashCode is positive
        (EqualityComparer.Default.GetHashCode key) &&& 0x7FFF_FFFF
        
    let computeSlotIndex (slotMask: int) (hashCode: int) =
        hashCode &&& slotMask
    
    type Internals<'Key, 'Value>() =
        member val Count = 0 with get, set
        member val Slots = Array.create 4 Slot.empty<'Key, 'Value> with get, set
        member val SlotMask = 4 - 1 with get, set

    [<RequireQualifiedAccess>]
    module Logic =
        
        let itemSet
            (key: 'Key)
            (value: 'Value)
            (internals: Internals< 'Key, 'Value>)
            : unit =
                
            let mutable slots = internals.Slots
            
            let rec loop (hashCode: int) (slotIdx: int) =
                if slotIdx < slots.Length then
                    // Check if slot is Empty or a Tombstone
                    if slots[slotIdx].IsAvailable then
                        slots[slotIdx].HashCode <- hashCode
                        slots[slotIdx].Key <- key
                        slots[slotIdx].Value <- value
                        1
                    else
                        // If we reach here, we know the slot is occupied
                        if EqualityComparer.Default.Equals (hashCode, slots[slotIdx].HashCode) &&
                            EqualityComparer.Default.Equals (key, slots[slotIdx].Key) then
                            slots[slotIdx].Value <- value
                            0
                        else
                            loop hashCode (slotIdx + 1)
                else
                    // Start over looking from the beginning of the slots
                    loop hashCode 0
                    
            let hashCode = computeHashCode key
            let slotIdx = computeSlotIndex internals.SlotMask hashCode
            internals.Count <- internals.Count + loop hashCode slotIdx
        
        
        let inline itemGetStruct
            (key: 'Key)
            (internals: Internals< 'Key, 'Value>)
            : 'Value =
            
            let slots = internals.Slots
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
                    
            let slotIdx = computeSlotIndex internals.SlotMask hashCode
            loop slotIdx
            
            
        let inline itemGetRef
            (key: 'Key)
            (internals: Internals< 'Key, 'Value>)
            : 'Value =
                
            let slots = internals.Slots
            
            let rec loop (hashCode: int) (slotIdx: int) =
                if slotIdx < slots.Length then
                    if slots[slotIdx].IsOccupied then
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
                    
            let avxStep (hashCode: int) (slotIdx: int) =
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
                        loop hashCode slotIdx
                else
                    loop hashCode slotIdx
                
            
            let hashCode = computeHashCode key
            let slotIdx = computeSlotIndex internals.SlotMask hashCode
            avxStep hashCode slotIdx
            
            
        let resize<'Key, 'Value> (internals: Internals< 'Key, 'Value>) =
            // Resize if our fill is >75%
            if internals.Count > (internals.Slots.Length >>> 2) * 3 then
            // if count > slots.Length - 2 then
                let oldSlots = internals.Slots
                
                // Increase the size of the backing store
                internals.Slots <- Array.create (oldSlots.Length <<< 1) Slot.empty
                internals.SlotMask <- internals.Slots.Length - 1
                internals.Count <- 0
                
                for slot in oldSlots do
                    if slot.IsEntry then
                        itemSet slot.Key slot.Value internals
                        
open Domain


type Dictionary<'Key, 'Value
    when 'Key : equality>
    (internals: Internals<'Key, 'Value>,
     itemGet: 'Key -> Internals<'Key, 'Value> -> 'Value) =
        
    member _.Internals = internals
    member _.ItemGet = itemGet
        
    member d.Item
        with inline get (key: 'Key) =
            d.ItemGet key d.Internals
            
        and inline set (key: 'Key) (value: 'Value) =
            Logic.itemSet key value d.Internals


module Dictionary =
    
    let ofSeq (entries: seq<'Key * 'Value>) =
        let internals = Internals()
        
        for key, value in entries do
            Logic.itemSet key value internals
            Logic.resize internals
        if typeof<'Key>.IsValueType then
            Dictionary (internals, Logic.itemGetStruct)
        else
            Dictionary (internals, Logic.itemGetRef)