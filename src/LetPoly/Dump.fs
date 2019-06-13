/// Dump terms as human-readable string.
module rec LetPoly.Dump

open LetPoly.Types

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

let tyIsAtom ty =
  match ty with
  | Ty.Any
  | Ty.Nat _ ->
    true
  | _ ->
    false

let termIsAtom term =
  match term with
  | Term.IntLit _
  | Term.Var _ ->
    true
  | _ ->
    false

let termIsApp term =
  match term with
  | Term.App _ ->
    true
  | _ ->
    false

let dumpErrors text errors acc =
  let rec go errors acc =
    match errors with
    | [] ->
      acc

    | SynError (msg, (l, r)) :: errors ->
      let (ly, lx), (ry, rx) = locateTextRangeFromTop text l r
      acc
      |> cons (sprintf "(%d, %d)..(%d, %d) ERROR\n" (ly + 1) (lx + 1) (ry + 1) (rx + 1))
      |> cons "  " |> cons msg |> cons "\n"
      |> go errors

  acc |> go errors

let dumpTy ty acc =
  let rec go ty acc =
    match ty with
    | Ty.Any ->
      acc |> cons "?"

    | Ty.Nat ->
      acc |> cons "Nat"

    | Ty.Fun (sTy, tTy) ->
      let acc =
        if tyIsAtom sTy then
          acc |> go sTy
        else
          acc |> cons "(" |> go sTy |> cons ")"
      let acc =
        acc |> cons " -> "
      let acc =
        if tyIsAtom tTy then
          acc |> go tTy
        else
          acc |> cons "(" |> go tTy |> cons ")"
      acc

  go ty acc

let dumpTerm (term, ty) acc =
  let rec go term acc =
    match term with
    | Term.IntLit (_, value) ->
      acc |> cons value

    | Term.Var (_, name, _, _) ->
      acc |> cons name

    | Term.Abs (_, name, body) ->
      acc |> cons "\\" |> cons name |> cons ". " |> go body

    | Term.App (_, cal, arg) ->
      let acc =
        if termIsAtom cal || termIsApp cal then
          acc |> go cal
        else
          acc |> cons "(" |> go cal |> cons ")"
      let acc =
        acc |> cons " "
      let acc =
        if termIsAtom arg then
          acc |> go arg
        else
          acc |> cons "(" |> go arg |> cons ")"
      acc

  acc |> go term |> cons " : " |> dumpTy ty

let dumpTerms terms acc =
  let rec go terms acc =
    match terms with
    | [] ->
      acc

    | term :: terms ->
      acc |> dumpTerm term |> cons "\n" |> go terms

  acc |> go terms

let dumpEnd acc =
  acc |> List.rev |> String.concat ""

let dump (text, terms, errors) =
  []
  |> dumpErrors text errors
  |> dumpTerms terms
  |> dumpEnd
