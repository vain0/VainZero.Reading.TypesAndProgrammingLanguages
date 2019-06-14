/// Name resolution.
module rec LetPoly.NameRes

open LetPoly.Types
open LetPoly.Helpers
open LetPoly.Syntax

// -----------------------------------------------
// Name Context
// -----------------------------------------------

let nameCtxLen nameCtx =
  List.length nameCtx

let nameCtxAdd (name: string) nameCtx =
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
    // Missing
    -1

// -----------------------------------------------
// Syntax to HIR
// -----------------------------------------------

let synToTerm (text: string, syn: Syn): Term option =
  let slice (l, r) = text |> strSlice l r
  let rec go syn =
    match syn with
    | Syn.Error _
    | Syn.Missing
    | Syn.Token _ ->
      None

    | Syn.Node (synId, kind, children) ->
      match kind, children with
      | SynKind.BoolLit, [Syn.Token (TokenKind.True, _)] ->
        Term.BoolLit (synId, true) |> Some

      | SynKind.BoolLit, [Syn.Token (TokenKind.False, _)] ->
        Term.BoolLit (synId, false) |> Some

      | SynKind.IntLit, [Syn.Token (TokenKind.IntLit, range)] ->
        let value = slice range
        Term.IntLit (synId, value) |> Some

      | SynKind.Var, [Syn.Token (TokenKind.Ident, range)] ->
        let name = slice range
        Term.Var (synId, name, noDbi, noCtxLen) |> Some

      | SynKind.Paren, [_parenL; body; _parenR] ->
        go body

      | SynKind.Abs, [_lambda; Syn.Token (TokenKind.Ident, range); _dot; body] ->
        let name = slice range
        match go body with
        | Some body ->
          Term.Abs (synId, name, body) |> Some
        | None ->
          None

      | SynKind.App, [cal; arg] ->
        match go cal, go arg with
        | Some cal, Some arg ->
          Term.App (synId, cal, arg) |> Some
        | _ ->
          None

      | _ ->
        /// FIXME: nested `semi` should cause an error
        None

  go syn

let synToCommands (text: string, syn: Syn): Command list =
  let rec go (syn, acc) =
    match syn with
    | Syn.Node (_, SynKind.Semi, [first; second]) ->
      let acc = go (first, acc)
      let acc = go (second, acc)
      acc

    | _ ->
      match synToTerm (text, syn) with
      | Some term ->
        Command.Eval term :: acc
      | None ->
        acc

  go (syn, []) |> List.rev

let nameResTerm term =
  let rec go nameCtx term =
    match term with
    | Term.BoolLit _
    | Term.IntLit _ ->
      term

    | Term.Var (id, name, _, _) ->
      let dbi = nameCtx |> nameCtxPos name
      let ctxLen = nameCtx |> nameCtxLen
      Term.Var (id, name, dbi, ctxLen)

    | Term.Abs (id, name, body) ->
      let nameCtx = name :: nameCtx
      let body = go nameCtx body
      Term.Abs (id, name, body)

    | Term.App (id, cal, arg) ->
      let cal = go nameCtx cal
      let arg = go nameCtx arg
      Term.App (id, cal, arg)

  go [] term

let nameRes commands =
  commands |> List.map (fun command ->
    match command with
    | Command.Eval term ->
      nameResTerm term |> Command.Eval
  )

let synToHir (text: string, syn: Syn) =
  synToCommands (text, syn) |> nameRes
