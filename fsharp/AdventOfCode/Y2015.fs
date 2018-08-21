namespace AdventOfCode

open LanguagePrimitives
open Microsoft.FSharp.Collections
open System
open System.Text

module Y2015 =

    let private puzzle = {year = Y2015; day = D01}

    module D01 =

        let puzzle = {puzzle with day = D01}

        let private parseMove = function
            | '(' -> 1
            | ')' -> -1
            | c -> puzzle.FailWith (sprintf "unexpected character %c" c)

        let solve() =
            let floors =
                puzzle.ReadAllText()
                |> Seq.map parseMove
                |> Seq.scan (+) 0
            let destFloor = Seq.last floors
            let basementIdx = Seq.findIndex ((=) -1) floors
            (destFloor, basementIdx)

    module D02 =

        let puzzle = {puzzle with day = D02}

        let private parseGift (line: string) =
            line.Split 'x'
            |> Array.map ParseInt32
            |> Array.sort
            |> function | [|a; b; c|] -> (a, b, c)
                        | _ -> puzzle.FailWith "wrong input format"

        let private giftPaperArea (a, b, c) = 3 * a * b + 2 * (a * c + b * c)

        let private giftRibbonLen (a, b, c) = 2 * (a + b) + a * b * c

        let solve() =
            let gifts = puzzle.ReadAllLines() |> Array.map parseGift
            let paperArea = Array.sumBy giftPaperArea gifts
            let ribbonLen = Array.sumBy giftRibbonLen gifts
            (paperArea, ribbonLen)

    module D03 =

        let puzzle = {puzzle with day = D03}

        let private parseMove = function
            | '>' -> (1, 0)
            | '<' -> (-1, 0)
            | '^' -> (0, 1)
            | 'v' -> (0, -1)
            | c -> puzzle.FailWith (sprintf "unexpected character %c" c)

        let private houses =
            let move (x, y) (dx, dy) = (x + dx, y + dy)
            Array.scan move (0, 0)

        let private countSanta = houses >> Array.distinct >> Array.length

        let private countSantaRobo =
            Array.split >> Tuple.both houses >> Tuple.uncurry Array.append
            >> Array.distinct >> Array.length

        let solve() =
            let moves =
                puzzle.ReadAllText()
                |> Seq.toArray
                |> Array.map parseMove
            let nSanta = countSanta moves
            let nSantaRobo = countSantaRobo moves
            (nSanta, nSantaRobo)

    module D04 =

        let puzzle = {puzzle with day = D04}

        let private findSuffix secret (prefix: string) start =
            let toMd5 =
                string >> Encoding.ASCII.GetBytes
                >> Array.append secret >> Hash.md5
            let rec go n =
                let md5 = toMd5 n
                if md5.StartsWith prefix then (n, md5) else go (n + 1)
            go start

        let solve() =
            let secret = puzzle.ReadAllBytes()
            let (n50s, _) = findSuffix secret "00000" 0
            let (n60s, _) = findSuffix secret "000000" n50s
            (n50s, n60s)

    module D05 =

        let puzzle = {puzzle with day = D05}

        let private rule1 (str: string) =
            str
            |> String.filter (fun c -> String.exists ((=) c) "aeiou")
            |> String.length
            |> ((<) 2)

        let private rule2 (str: string) =
            str
            |> Seq.windowed 2
            |> Seq.exists (function
                | [|c1; c2|] -> c1 = c2
                | _ -> puzzle.FailWith "unreachable in rule2")

        let private rule3 (str: string) =
            not <| Array.exists (str.Contains: string -> bool) [|"ab"; "cd"; "pq"; "xy"|]

        let private isNice1 (str: string) =
            rule1 str && rule2 str && rule3 str

        let private rule4 (str: string) =
            str
            |> Seq.windowed 2
            |> Seq.take (str.Length - 3)
            |> Seq.exists (function
                | [|c1; c2|] ->
                    let pair = String.Format("{0}{1}", c1, c2)
                    let splits = str.Split([|pair|], StringSplitOptions.None)
                    splits.Length > 2
                | _ -> false)

        let private rule5 (str: string) =
            str |> Seq.skip 2 |> Seq.zip str |> Seq.exists ((<||) (=))

        let private isNice2 (str: string)  =
            rule4 str && rule5 str

        let solve() =
            let strings = puzzle.ReadAllLines()
            let n1 = strings |> Array.filter isNice1 |> Array.length
            let n2 = strings |> Array.filter isNice2 |> Array.length
            (n1, n2)

    module D06 =

        let puzzle = {puzzle with day = D06}

        type private Action = TurnOff | Toggle | TurnOn

        [<StructuredFormatDisplay("Instruction \{action = {action}; li = {li}; lj = {lj}; ui = {ui}; uj = {uj}\}")>]
        type private Instruction = {
            action: Action
            li: int
            lj: int
            ui: int
            uj: int
        }

        let private parseRest (str: string) =
            str.Split([|"through"; ","|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun nstr -> nstr.Trim() |> ParseInt32)
            |> function
                | [|li; lj; ui; uj|] -> (li, lj, ui, uj)
                | _ -> puzzle.FailWith "wrong input format in parseRest"

        let private parseInstruction (str: string) =
            let (action, (li, lj, ui, uj)) =
                if str.StartsWith("turn on") then
                    (TurnOn, parseRest (str.Replace("turn on", "")))
                elif str.StartsWith("toggle") then
                    (Toggle, parseRest (str.Replace("toggle", "")))
                elif str.StartsWith("turn off") then
                    (TurnOff, parseRest (str.Replace("turn off", "")))
                else puzzle.FailWith "wrong input format in parseInstruction"
            {action = action; li = li; lj = lj; ui = ui; uj = uj}

        let private mkGrid (instructions: Instruction []) =
            let mutable grid = Array2D.create 1000 1000 (false, 0)
            let updateGrid action i j =
                let (state, brightness) = grid.[i, j]
                match action with
                | TurnOn -> grid.[i, j] <- (true, brightness + 1)
                | TurnOff -> grid.[i, j] <- (false, max 0 (brightness - 1))
                | Toggle -> grid.[i, j] <- (not state, brightness + 2)
            for {action = action; li = li; lj = lj; ui = ui; uj = uj} in instructions do
                for i in li .. ui do
                    for j in lj .. uj do
                        updateGrid action i j
            grid

        let solve() =
            let instructions = puzzle.ReadAllLines() |> Array.map parseInstruction
            let grid = mkGrid instructions
            let lit = ref 0
            let brightnessTotal = ref 0
            Array2D.iter (fun (state, brightness) ->
                if state then lit := !lit + 1
                brightnessTotal := !brightnessTotal + brightness
            ) grid
            (lit, brightnessTotal)

    module D07 =

        let puzzle = {puzzle with day = D07}

        let solve() =
            (0,0)
