namespace AdventOfCode

// module Solve =
//     type Puzzle with
//         member this.Solve() =
//             match this.year with
//             | Y2015 ->
//                 match this.day with
//                 | D01 -> Some(Y2015.D01.solve())
//                 | D02 -> Some(Y2015.D02.solve())
//                 | _ -> None
//             | _ -> None

module Main =
    open Y2015

    [<EntryPoint>]
    let main _argv =
        printfn "««««««««««««««««««««««««««««»»»»»»»»»»»»»»»»»»»»»»»»»»»»"
        printfn "»»»»»»»»»»»»»»»»»»»» Advent Of Code ««««««««««««««««««««"
        printfn "««««««««««««««««««««««««««««»»»»»»»»»»»»»»»»»»»»»»»»»»»»"
        printfn ""
        printfn "%A" (D01.solve())
        printfn "%A" (D02.solve())
        printfn "%A" (D03.solve())
        printfn "%A" (D04.solve())
        printfn "%A" (D05.solve())
        printfn "%A" (D06.solve())
        printfn "%A" (D07.solve())
        printfn ""
        printfn "««««««««««««««««««««««««««««»»»»»»»»»»»»»»»»»»»»»»»»»»»»"
        printfn "««««««««««««««««««««««««««««»»»»»»»»»»»»»»»»»»»»»»»»»»»»"
        0 // return an integer exit code
