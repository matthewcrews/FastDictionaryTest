[<Struct>]
type Chicken =
    {
        Name: string
        mutable Parent: Option<Chicken>
    }


// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let mutable c = { Name = "Clucky"; Parent = None }

printfn $"{c}"

let c2 = { Name = "C2"; Parent = Unchecked.defaultof<_> }
c.Parent <- Some c2

printfn $"{c}"

()