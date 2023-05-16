module FastDictionaryTest.SOA.StaticDict

open System
open FastDictionaryTest.SOA.Helpers

let create (entries: seq<'Key * 'Value>) =

    match entries with
    | :? seq<int * 'Value> as entries ->
        IntDict.create entries :?> IStaticDictionary<'Key, 'Value>
    | :? seq<string * 'Value> as entries ->
        StrDict.create entries :?> IStaticDictionary<'Key, 'Value>
    | _ ->
        raise (NotImplementedException())
