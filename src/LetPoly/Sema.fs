/// Semantic analysis.
[<AutoOpen>]
module rec LetPoly.Sema

let nameCtxLen nameCtx =
  List.length nameCtx

let nameCtxAdd name nameCtx =
  if nameCtx |> List.contains name then
    nameCtx
  else
    name :: nameCtx

let nameCtxNth (dbi: DeBruijnIndex) nameCtx =
  nameCtx |> List.item dbi

let rec nameCtxPos name nameCtx =
  match nameCtx with
  | top :: _ when top = name ->
    0
  | _ :: nameCtx ->
    1 + nameCtxPos name nameCtx
  | _ ->
    -1

let astToFreeVars ast =
  let rec go nameCtx ast fvs =
    match ast with
    | Ast.Var name ->
      if nameCtxPos name nameCtx < 0 then
        name :: fvs
      else
        fvs

    | Ast.Abs (name, body) ->
      let nameCtx = name :: nameCtx
      fvs |> go nameCtx body

    | Ast.App (cal, arg) ->
      fvs |> go nameCtx cal |> go nameCtx arg

    | Ast.Semi (first, second) ->
      fvs |> go nameCtx first |> go nameCtx second

  go [] ast [] |> set |> Set.toList

let indexToNamedAst nameCtx iast =
  match iast with
  | IndexAst.Var (dbi, ctxLen) ->
    let name = nameCtx  |> nameCtxNth dbi
    Ast.Var name

  | IndexAst.Abs (name, body) ->
    let nameCtx = name :: nameCtx
    let body = indexToNamedAst nameCtx body
    Ast.Abs (name, body)

  | IndexAst.App (cal, arg) ->
    let cal = indexToNamedAst nameCtx cal
    let arg = indexToNamedAst nameCtx arg
    Ast.App (cal, arg)

  | IndexAst.Semi (first, second) ->
    let first = indexToNamedAst nameCtx first
    let second = indexToNamedAst nameCtx second
    Ast.Semi (first, second)

let namedToIndexAst nameCtx ast =
  match ast with
  | Ast.Var name ->
    let dbi = nameCtx |> nameCtxPos name
    let ctxLen = nameCtx |> nameCtxLen
    IndexAst.Var (dbi, ctxLen)

  | Ast.Abs (name, body) ->
    let nameCtx = name :: nameCtx
    let body = namedToIndexAst nameCtx body
    IndexAst.Abs (name, body)

  | Ast.App (cal, arg) ->
    let cal = namedToIndexAst nameCtx cal
    let arg = namedToIndexAst nameCtx arg
    IndexAst.App (cal, arg)

  | Ast.Semi (first, second) ->
    let first = namedToIndexAst nameCtx first
    let second = namedToIndexAst nameCtx second
    IndexAst.Semi (first, second)

let indexAstToTerm serial iast =
  let termId, serial = serial, serial + 1

  match iast with
  | IndexAst.Var (dbi, ctxLen) ->
    Term.Var (termId, dbi, ctxLen), serial

  | IndexAst.Abs (name, body) ->
    let body, serial = indexAstToTerm serial body
    Term.Abs (termId, name, body), serial

  | IndexAst.App (cal, arg) ->
    let cal, serial = indexAstToTerm serial cal
    let arg, serial = indexAstToTerm serial arg
    Term.App (termId, cal, arg), serial

  | IndexAst.Semi _ ->
    failwith "Invalid use of semi"

let termToIndexAst term =
  match term with
  | Term.Var (_, dbi, ctxLen) ->
    IndexAst.Var (dbi, ctxLen)

  | Term.Abs (_, name, body) ->
    let body = termToIndexAst body
    IndexAst.Abs (name, body)

  | Term.App (_, cal, arg) ->
    let cal = termToIndexAst cal
    let arg = termToIndexAst arg
    IndexAst.App (cal, arg)

let indexAstToCommands acc (iast, serial) =
  match iast with
  | IndexAst.Semi (first, second) ->
    let acc, serial = (first, serial) |> indexAstToCommands acc
    let acc, serial = (second, serial) |> indexAstToCommands acc
    acc, serial

  | _ ->
    let term, serial = indexAstToTerm serial iast
    Command.Eval term :: acc, serial

let astToCommands (text: string, ast: Ast option, errors: SynError list): string * Command list * SynError list * NameContext * int =
  let serial = 0

  match ast with
  | None ->
    text, [], errors, [], serial

  | Some ast ->
    let nameCtx = ast |> astToFreeVars
    let iast = ast |> namedToIndexAst nameCtx
    let acc, serial = indexAstToCommands [] (iast, serial)
    let commands = List.rev acc
    text, commands, errors, nameCtx, serial
