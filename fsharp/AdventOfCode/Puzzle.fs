namespace AdventOfCode

type Year =
    | Y2015 | Y2016 | Y2017

    member this.Value =
        match this with
        | Y2015 -> 2015
        | Y2016 -> 2016
        | Y2017 -> 2017

    override this.ToString() = sprintf "%d" this.Value

type Day =
    | D01 | D02 | D03 | D04 | D05 | D06 | D07 | D08 | D09 | D10
    | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20
    | D21 | D22 | D23 | D24 | D25

    member this.Value =
        match this with
        | D01 -> 1  | D02 -> 2  | D03 -> 3  | D04 -> 4  | D05 -> 5
        | D06 -> 6  | D07 -> 7  | D08 -> 8  | D09 -> 9  | D10 -> 10
        | D11 -> 11 | D12 -> 12 | D13 -> 13 | D14 -> 14 | D15 -> 15
        | D16 -> 16 | D17 -> 17 | D18 -> 18 | D19 -> 19 | D20 -> 20
        | D21 -> 21 | D22 -> 22 | D23 -> 23 | D24 -> 24 | D25 -> 25

    override this.ToString() = sprintf "%02d" this.Value

[<StructuredFormatDisplay("Puzzle \{year = {year}; day = {day}\}")>]
type Puzzle =
    { year: Year;
      day: Day;
    }
    override this.ToString() = sprintf "Puzzle %O %O" this.year this.day

[<AutoOpen>]
module PuzzleIO =
    open System.IO
    open System.Diagnostics

    type Year with
        member this.DirName = sprintf "%d" this.Value

    type Day with
        member this.Dirname = sprintf "%02d" this.Value

    let private rootDir =
        let procStartInfo =
            ProcessStartInfo(
                FileName = "git",
                Arguments = "rev-parse --show-toplevel",
                UseShellExecute = false,
                RedirectStandardOutput = true
            )
        using (new Process(StartInfo = procStartInfo)) (fun proc ->
            proc.Start() |> ignore
            let output = proc.StandardOutput.ReadLine()
            proc.WaitForExit()
            output
        )

    let private puzzleDir = Path.Combine(rootDir, "puzzles")

    let private puzzlePath (year: Year) (day: Day) (filename: string) =
        Path.Combine(puzzleDir, year.DirName, day.Dirname, filename)

    type Puzzle with
        member this.ReadAllText (?filename: string) =
            let filename = defaultArg filename "input"
            puzzlePath this.year this.day filename |> File.ReadAllText

        member this.ReadAllLines (?filename: string) =
            let filename = defaultArg filename "input"
            puzzlePath this.year this.day filename |> File.ReadAllLines

        member this.ReadAllBytes (?filename: string) =
            let filename = defaultArg filename "input"
            puzzlePath this.year this.day filename |> File.ReadAllBytes

        member this.FailWith (message: string) =
            failwith <| sprintf "error (%O): %s\n" this message
