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
    | Ast.TyVar _
    | Ast.IntLit _ ->
      fvs

    | Ast.Var name ->
      if nameCtxPos name nameCtx < 0 then
        name :: fvs
      else
        fvs

    | Ast.Abs (name, _, body) ->
      let nameCtx = name :: nameCtx
      fvs |> go nameCtx body

    | Ast.App (cal, arg) ->
      fvs |> go nameCtx cal |> go nameCtx arg

    | Ast.Semi (first, second) ->
      fvs |> go nameCtx first |> go nameCtx second

  go [] ast [] |> set |> Set.toList

let indexToNamedAst tyNameCtx nameCtx iast =
  match iast with
  | IndexAst.TyVar dbi ->
    let name = tyNameCtx |> nameCtxNth dbi
    Ast.TyVar name

  | IndexAst.IntLit value ->
    Ast.IntLit value

  | IndexAst.Var (dbi, ctxLen) ->
    let name = nameCtx |> nameCtxNth dbi
    Ast.Var name

  | IndexAst.Abs (name, ty, body) ->
    let nameCtx = name :: nameCtx
    let ty = indexToNamedAst tyNameCtx nameCtx ty
    let body = indexToNamedAst tyNameCtx nameCtx body
    Ast.Abs (name, ty, body)

  | IndexAst.App (cal, arg) ->
    let cal = indexToNamedAst tyNameCtx nameCtx cal
    let arg = indexToNamedAst tyNameCtx nameCtx arg
    Ast.App (cal, arg)

  | IndexAst.Semi (first, second) ->
    let first = indexToNamedAst tyNameCtx nameCtx first
    let second = indexToNamedAst tyNameCtx nameCtx second
    Ast.Semi (first, second)

let namedToIndexAst tyNameCtx nameCtx ast =
  match ast with
  | Ast.TyVar name ->
    let dbi = tyNameCtx |> nameCtxPos name
    IndexAst.TyVar dbi

  | Ast.IntLit value ->
    IndexAst.IntLit value

  | Ast.Var name ->
    let dbi = nameCtx |> nameCtxPos name
    let ctxLen = nameCtx |> nameCtxLen
    IndexAst.Var (dbi, ctxLen)

  | Ast.Abs (name, ty, body) ->
    let nameCtx = name :: nameCtx
    let ty = namedToIndexAst tyNameCtx nameCtx ty
    let body = namedToIndexAst tyNameCtx nameCtx body
    IndexAst.Abs (name, ty, body)

  | Ast.App (cal, arg) ->
    let cal = namedToIndexAst tyNameCtx nameCtx cal
    let arg = namedToIndexAst tyNameCtx nameCtx arg
    IndexAst.App (cal, arg)

  | Ast.Semi (first, second) ->
    let first = namedToIndexAst tyNameCtx nameCtx first
    let second = namedToIndexAst tyNameCtx nameCtx second
    IndexAst.Semi (first, second)

let indexAstToTy serial iast =
  match iast with
  | IndexAst.TyVar dbi ->
    Ty.Con dbi, serial

  | _ ->
    failwith "NEVER"

let indexAstToTerm serial iast =
  let termId, serial = serial, serial + 1

  match iast with
  | IndexAst.TyVar dbi ->
    failwith "NEVER"

  | IndexAst.IntLit value ->
    Term.IntLit (termId, value), serial

  | IndexAst.Var (dbi, ctxLen) ->
    Term.Var (termId, dbi, ctxLen), serial

  | IndexAst.Abs (name, ty, body) ->
    let ty, serial = indexAstToTerm serial ty
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
  | Term.IntLit (_, value) ->
    IndexAst.IntLit value

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
