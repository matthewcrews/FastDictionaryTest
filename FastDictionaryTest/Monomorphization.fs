namespace FastDictionaryTest.Monomorphization

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
    
    [<Struct>]
    type Internals<'Key, 'Value> =
        {
            mutable Count : int
            mutable Slots : Slot<'Key, 'Value>[]
            mutable SlotBitShift : int
        }
        
    module Internals =
        
        let empty<'Key, 'Value> =
            let initialCapacity = 4
            {
                Count = 0
                Slots = Array.create initialCapacity Slot.empty<'Key, 'Value>
                SlotBitShift = 64 - (System.Numerics.BitOperations.TrailingZeroCount initialCapacity)
            }

    [<RequireQualifiedAccess>]
    type Logic =
        
        static member AddEntry< 'Key, 'Value> (key: 'Key, value: 'Value, internals: byref<Internals< 'Key, 'Value>>) =
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
            let slotIdx = computeSlotIndex internals.SlotBitShift hashCode
            internals.Count <- internals.Count + loop hashCode slotIdx
        
        
        static member GetValue<'Key, 'Value when 'Key: struct> (key: 'Key, internals: inref<Internals< 'Key, 'Value>>, ?priority: IPriority1) : 'Value =
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
                    
            let slotIdx = computeSlotIndex internals.SlotBitShift hashCode
            loop slotIdx
            
            
        static member GetValue<'Key, 'Value when 'Key: not struct>(key: 'Key, internals: inref<Internals< 'Key, 'Value>>, ?priority: IPriority2) : 'Value =
            
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
                    
            let slotIdx = computeSlotIndex internals.SlotBitShift hashCode
            avxStep slotIdx
            
            
        static member Resize<'Key, 'Value> (internals: byref<Internals< 'Key, 'Value>>) =
            // Resize if our fill is >75%
            if internals.Count > (internals.Slots.Length >>> 2) * 3 then
            // if count > slots.Length - 2 then
                let oldSlots = internals.Slots
                
                // Increase the size of the backing store
                internals.Slots <- Array.create (oldSlots.Length <<< 1) Slot.empty
                internals.SlotBitShift <- 64 - (System.Numerics.BitOperations.TrailingZeroCount internals.Slots.Length)
                internals.Count <- 0
                
                for slot in oldSlots do
                    if slot.IsEntry then
                        Logic.AddEntry (slot.Key, slot.Value, &internals)
                        
open Domain

type Dictionary<'KeyS, 'KeyR, 'Value
    when 'KeyS : equality
    and 'KeyS: struct
    and 'KeyR : equality
    and 'KeyR: not struct>
    private (internalsS: Internals<'KeyS, 'Value>, internalsR: Internals<'KeyR, 'Value>) =
    
    
    static member ofSeq (entries: seq<'KeyS * 'Value>) =
        let mutable internalsS : Internals<'KeyS, 'Value> = Internals.empty
        let mutable internalsR : Internals<'KeyR, 'Value> = Unchecked.defaultof<_>
        do
            for k, v in entries do
                Logic.AddEntry (k, v, &internalsS)
                Logic.Resize &internalsS
                
        Dictionary<'KeyS, 'KeyR, 'Value>(internalsS, internalsR)
        
    static member ofSeq (entries: seq<'KeyR * 'Value>) =
        let mutable internalsS : Internals<'KeyS, 'Value> = Unchecked.defaultof<_>
        let mutable internalsR : Internals<'KeyR, 'Value> = Internals.empty
        do
            for k, v in entries do
                Logic.AddEntry (k, v, &internalsR)
                Logic.Resize &internalsR
                
        Dictionary<'KeyS, 'KeyR, 'Value>(internalsS, internalsR)
        
        
    member d.Item
        with get (key: 'KeyS) =
            Logic.GetValue (key, &internalsS)
            
        and set (key: 'KeyS) (value: 'Value) =
                let mutable internals = internalsS
                Logic.AddEntry (key, value, &internals)
                Logic.Resize &internals

    member d.Item
        with get (key: 'KeyR) =
            Logic.GetValue (key, &internalsR)

        and set (key: 'KeyR) (value: 'Value) =
                let mutable internals = internalsR
                Logic.AddEntry (key, value, &internals)
                Logic.Resize &internals
