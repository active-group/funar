open System
open Library

[<EntryPoint>]
let main args =
    printfn "Nice command-line arguments! Here's what System.Text.Json has to say about them:"

    let value, json = getJson {| args=args; year=System.DateTime.Now.Year |}
    printfn $"Input: %0A{value}"
    printfn $"Output: %s{json}"

    0 // return an integer exit code