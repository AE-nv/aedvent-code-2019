#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

printf "Testing..."

printfn "..done!"

let digits (s : string) =
    s.Trim() |> Seq.toList |> List.map (string >> (fun x -> printfn "%A" x; x) >>int)

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
input |> digits