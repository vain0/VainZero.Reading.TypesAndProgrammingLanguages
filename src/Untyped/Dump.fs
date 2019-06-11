/// Dump terms as human-readable string.
module rec Untyped.Dump

open Untyped.Types

let cons head tail =
  head :: tail

let locateTextPos (y, x) (text: string) (l: int) (r: int) =
  let rec go y x i =
    if i = r then
      y, x
    else if text.[i] = '\n' then
      go (y + 1) 0 (i + 1)
    else
      go y (x + 1) (i + 1)
  go y x l

let locateTextRangeFromTop (text: string) (l: int) (r: int): TextRange * TextRange =
  let ly, lx = locateTextPos (0, 0) text 0 l
  let ry, rx = locateTextPos (ly, lx) text l r
  (ly, lx), (ry, rx)

let dumpErrors text errors acc =
  let rec go errors acc =
    match errors with
    | [] ->
      acc

    | SynErr (msg, (l, r)) :: errors ->
      let (ly, lx), (ry, rx) = locateTextRangeFromTop text l r
      acc
      |> cons (sprintf "(%d, %d)..(%d, %d) ERROR\n" (ly + 1) (lx + 1) (ry + 1) (rx + 1))
      |> cons "  " |> cons msg |> cons "\n"
      |> go errors

  acc |> go errors

let dumpTerm nameCtx term acc =
  let ast = term |> termToIndexAst |> indexToNamedAst nameCtx

  let isVar ast =
    match ast with
    | Ast.Var _ ->
      true
    | _ ->
      false

  let isApp ast =
    match ast with
    | Ast.App _ ->
      true
    | _ ->
      false

  let rec go ast acc =
    match ast with
    | Ast.Var name ->
      acc |> cons name

    | Ast.Abs (name, body) ->
      acc |> cons "\\" |> cons name |> cons ". " |> go body

    | Ast.App (cal, arg) ->
      let acc =
        if isApp cal || isVar cal then
          acc |> go cal
        else
          acc |> cons "(" |> go cal |> cons ")"
      let acc =
        acc |> cons " "
      let acc =
        if isVar arg then
          acc |> go arg
        else
          acc |> cons "(" |> go arg |> cons ")"
      acc

    | Ast.Semi (first, second) ->
      acc |> go first |> cons ";\n" |> go second

  acc |> go ast

let dumpTerms nameCtx terms acc =
  let rec go terms acc =
    match terms with
    | [] ->
      acc

    | term :: terms ->
      acc |> dumpTerm nameCtx term |> cons "\n" |> go terms

  go terms acc

let dumpEnd acc =
  acc |> List.rev |> String.concat ""

let dump (text, terms, errors, nameCtx) =
  []
  |> dumpErrors text errors
  |> dumpTerms nameCtx terms
  |> dumpEnd
