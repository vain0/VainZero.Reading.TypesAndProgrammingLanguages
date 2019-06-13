/// Evaluate terms.
module rec LetPoly.Eval

type EvalContext = unit

/// f: cut -> varTermId -> name -> deBruijnIndex -> contextLength -> term
let termMap f c term =
  let rec go c term =
    match term with
    | Term.IntLit _ ->
      term

    | Term.Var (termId, name, dbi, ctxLen) ->
      f c termId name dbi ctxLen

    | Term.Abs (termId, name, body) ->
      let body = go (c + 1) body
      Term.Abs (termId, name, body)

    | Term.App (termId, cal, arg) ->
      let cal = go c cal
      let arg = go c arg
      Term.App (termId, cal, arg)

  go c term

let termShiftAbove (d: int) c term =
  let f c termId name dbi ctxLen =
    if dbi >= c then
      Term.Var (termId, name, dbi + d, ctxLen + d)
    else
      Term.Var (termId, name, dbi, ctxLen + d)

  termMap f c term

let termShift (d: int) (term: Term) =
  termShiftAbove d 0 term

let termSubst j newTerm term =
  let f c termId name dbi ctxLen =
    if dbi = j + c then
      termShift c newTerm
    else
      // Unchanged.
      Term.Var (termId, name, dbi, ctxLen)

  termMap f 0 term

let termSubstTop newTerm term =
  termShift (-1) (termSubst 0 (termShift 1 newTerm) term)

let termIsVal _ term =
  match term with
  | Term.Abs _ ->
    true
  | _ ->
    false

let evalTerm (term, ctx) =
  let reduce term =
    match term with
    | Term.App (_, Term.Abs (_, _, t12), v2)
      when termIsVal ctx v2 ->
      termSubstTop v2 t12

    | Term.App (id, v1, t2)
      when termIsVal ctx v1 ->
      let t2 = evalTerm (t2, ctx)
      Term.App (id, v1, t2)

    | Term.App (id, t1, t2) ->
      let t1 = evalTerm (t1, ctx)
      Term.App (id, t1, t2)

    | _ ->
      term

  let rec go term =
    let nextTerm = reduce term
    if System.Object.ReferenceEquals(nextTerm, term) then
      term
    else
      go nextTerm

  go term

let evalCommands (commands: Command list, ctx: EvalContext): Term list * EvalContext =
  let rec go acc ctx commands =
    match commands with
    | [] ->
      List.rev acc, ctx

    | Command.Eval term :: commands ->
      let term = evalTerm (term, ctx)
      go (term :: acc) ctx commands

  go [] ctx commands

let eval (commands: Command list) =
  let ctx = ()
  let terms, _ = evalCommands (commands, ctx)
  terms
