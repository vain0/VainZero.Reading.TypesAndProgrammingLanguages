module rec LetPoly.Program

open LetPoly.Types
open LetPoly.Syntax
open LetPoly.NameRes
open LetPoly.Eval
open LetPoly.Dump

let run (text: string) =
  let serial = 0
  let text, syn, errors, serial = (text, serial) |> synBuild
  let commands = synToHir (text, syn)
  let terms = eval commands
  let output = dump (text, terms, errors)
  output

[<EntryPoint>]
let main argv =
  let fileName =
    match argv with
    | [|fileName|] ->
      fileName
    | _ ->
      failwith "USAGE: LetPoly <file-name>"
  let text =
    System.IO.File.ReadAllText(fileName)

  printf "%s" (run text)
  0
