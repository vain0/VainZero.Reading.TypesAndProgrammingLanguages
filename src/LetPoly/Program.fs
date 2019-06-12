module rec LetPoly.Program

open LetPoly.Types
open LetPoly.Tokenize
open LetPoly.Parse
open LetPoly.Sema
open LetPoly.Eval
open LetPoly.Dump

let run (text: string) =
  text |> tokenize |> parse |> synToAst |> astToCommands |> eval |> dump

[<EntryPoint>]
let main argv =
  let fileName =
    match argv with
    | [|fileName|] ->
      fileName
    | _ ->
      failwith "USAGE: untyped <file-name>"
  let text =
    System.IO.File.ReadAllText(fileName)

  printf "%s" (run text)
  0
