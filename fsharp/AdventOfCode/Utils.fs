namespace AdventOfCode

open System.Security.Cryptography
open System.Text

module Array =

    let split array =
        let (n, r) = (Array.length array / 2, Array.length array % 2)
        let array1 = Array.init (n + r) (fun i -> array.[2 * i])
        let array2 = Array.init n (fun i -> array.[2 * i + 1])
        (array1, array2)

module List =

    let rec tails = function
        | [] -> [[]]
        | (_ :: rest) as lst -> lst :: tails rest

//     let rec prependToAll sep = function
//         | [] -> []
//         | x :: xs -> sep :: x :: prependToAll sep xs

//     let intersperse sep = function
//         | [] -> []
//         | x :: xs -> x :: prependToAll sep xs

//     let intercalate xs xss = List.concat (intersperse xs xss)

module Hash =

    let md5 (data: byte []) =
        use md5 = MD5.Create()
        (StringBuilder(32), md5.ComputeHash(data))
        ||> Array.fold (fun sb b -> sb.Append(b.ToString("X2")))
        |> string

    let md5String (input: string) = md5 (Encoding.ASCII.GetBytes input)

module Tuple =

    let both f (a, b) = (f a, f b)

    let curry f a b = f (a, b)

    let uncurry = (<||)
