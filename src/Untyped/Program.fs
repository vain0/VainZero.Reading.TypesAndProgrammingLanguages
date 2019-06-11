module rec Untyped.Program

open Untyped.Types
open Untyped.Tokenize
open Untyped.Parse
open Untyped.Sema
open Untyped.Eval
open Untyped.Dump

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
