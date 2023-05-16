module FastDictionaryTest.SOA.StaticDict

open FastDictionaryTest.SOA.Helpers

let create (entries: seq<'Key * 'Value>) : StaticDict<'Key, 'Value> =

    match entries with
    | :? seq<int * 'Value> as entries ->
        retype (IntDict.create entries)
    | :? seq<string * 'Value> as entries ->
        retype (StrDict.create entries)
    | _ ->
        if typeof<'Key>.IsValueType then
            ValueDict.create entries
        else
            RefDict.create entries
