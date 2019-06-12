/// Syntactical analysis.
module rec LetPoly.Parse

open LetPoly.Types

// -----------------------------------------------
// Helpers
// -----------------------------------------------

let noRange: TextRange = 0, 0

let nextRange tokens: TextRange =
  match tokens with
  | (_, range) :: _ ->
    range
  | _ ->
    noRange

// -----------------------------------------------
// Eat tokens
// -----------------------------------------------

let eatIdent tokens =
  match tokens with
  | (TokenKind.Ident, _) as token :: tokens ->
    Syn.Token token, tokens
  | _ ->
    Syn.Error ("Expected an identifier", nextRange tokens, []), tokens

let eatDot tokens =
  match tokens with
  | (TokenKind.Dot, _) as token :: tokens ->
    Syn.Token token, tokens
  | _ ->
    Syn.Error ("Expected '.'", nextRange tokens, []), tokens

let eatColon tokens =
  match tokens with
  | (TokenKind.Colon, _) as token :: tokens ->
    Syn.Token token, tokens
  | _ ->
    Syn.Error ("Expected ':'", nextRange tokens, []), tokens

let eatParenR leftParenRange tokens =
  match tokens with
  | (TokenKind.ParenR, _) as token :: tokens ->
    Syn.Token token, tokens
  | _ ->
    let syn = Syn.Error ("Not closed by ')'", leftParenRange, [])
    syn, tokens

/// Eat a `;` and remove trailing semicolons.
let eatSemi tokens =
  let rec skipSemiTokens tokens =
    match tokens with
    | (TokenKind.Semi, _) :: tokens ->
      skipSemiTokens tokens
    | _ ->
      tokens

  match tokens with
  | (TokenKind.Semi, _) as semi :: tokens ->
    Syn.Token semi, skipSemiTokens tokens
  | _ ->
    let semi = Syn.Error ("Expected ';'", nextRange tokens, [])
    semi, tokens

// -----------------------------------------------
// Parse non-terminals
// -----------------------------------------------

let parseTy tokens =
  match tokens with
  | (TokenKind.Ident, _) as token :: tokens ->
    Syn.Var (Syn.Token token), tokens

  | _ ->
    Syn.Error ("Expected a type", nextRange tokens, []), tokens

let parseAtom tokens =
  match tokens with
  | (TokenKind.IntLit, _) as token :: tokens ->
    Syn.IntLit (Syn.Token token), tokens

  | (TokenKind.Ident, _) as token :: tokens ->
    Syn.Var (Syn.Token token), tokens

  | (TokenKind.ParenL, range) as parenL :: tokens ->
    let term, tokens = parseTerm tokens
    let parenR, tokens = eatParenR range tokens
    Syn.Paren (Syn.Token parenL, term, parenR), tokens

  | (TokenKind.Lambda, _) as lambda :: tokens ->
    let ident, tokens = eatIdent tokens
    let colon, tokens = eatColon tokens
    let ty, tokens = parseTy tokens
    let dot, tokens = eatDot tokens
    let body, tokens = parseTerm tokens
    Syn.Abs (Syn.Token lambda, ident, colon, ty, dot, body), tokens

  | _ ->
    Syn.Error ("Expected a term", nextRange tokens, []), tokens

/// `app = atom+`
let parseApp tokens =
  let rec go cal tokens =
    match tokens with
    // `first(term) \ first(abs)`
    | (TokenKind.IntLit, _) :: _
    | (TokenKind.Ident, _) :: _
    | (TokenKind.ParenL, _) :: _ ->
      let arg, tokens = parseAtom tokens
      let cal = Syn.App (cal, arg)
      go cal tokens
    | _ ->
      cal, tokens

  let cal, tokens = parseAtom tokens
  let app, tokens = go cal tokens
  app, tokens

let rec parseTerm tokens =
  match tokens with
  | (TokenKind.IntLit, _) :: _
  | (TokenKind.Ident, _) :: _
  | (TokenKind.ParenL, _) :: _
  | (TokenKind.Lambda, _) :: _ ->
    parseApp tokens

  | (_, range) :: tokens ->
    let term, tokens = parseTerm tokens
    Syn.Error ("Unexpected token", range, [term]), tokens

  | [] ->
    Syn.Error ("Expected a term but EOF", noRange, []), tokens

/// `semi = term (';'+ term?)*`
let parseSemi tokens =
  let rec go first tokens =
    let semi, tokens = eatSemi tokens
    match tokens with
    | [] ->
      first, []
    | _ ->
      let second, tokens = parseTerm tokens
      let first = Syn.Semi (first, semi, second)
      go first tokens

  let term, tokens = parseTerm tokens
  go term tokens

let parseEof syn tokens =
  match tokens with
  | [] ->
    syn
  | _ ->
    Syn.Error ("Expected EOF", noRange, [syn])

let parseRoot tokens =
  let _, tokens = eatSemi tokens
  let syn, tokens = parseSemi tokens
  parseEof syn tokens

let parse (text: string, tokens: Token list): string * Syn =
  text, parseRoot tokens

// -----------------------------------------------
// Convert to abstract syntax tree
// -----------------------------------------------

let strSlice start endIndex (s: string) =
  if start >= endIndex then
    ""
  else
    s.[start..endIndex - 1]

let synToAst (text: string, syn: Syn): string * Ast option * SynError list =
  let rec go acc syn =
    match syn with
    | Syn.Error (msg, range, children) ->
      let acc = SynErr (msg, range) :: acc
      let acc = List.fold (fun acc syn -> go acc syn |> snd) acc children
      None, acc

    | Syn.Token _ ->
      None, acc

    | Syn.TyVar ident ->
      match ident with
      | Syn.Token (TokenKind.Ident, (l, r)) ->
        let ident = text |> strSlice l r
        Some (Ast.TyVar ident), acc
      | _ ->
        None, acc

    | Syn.Ty ty ->
      assert (match ty with Syn.TyVar _ -> true | _ -> false)
      go acc ty

    | Syn.IntLit intLit ->
      match intLit with
      | Syn.Token (TokenKind.IntLit, (l, r)) ->
        let value = text |> strSlice l r |> int // FIXME: error if parse error
        Some (Ast.IntLit value), acc
      | _ ->
        None, acc

    | Syn.Var ident ->
      match ident with
      | Syn.Token (TokenKind.Ident, (l, r)) ->
        let ident = text |> strSlice l r
        Some (Ast.Var ident), acc
      | _ ->
        None, acc

    | Syn.Paren (parenL, body, parenR) ->
      let _, acc = go acc parenL
      let bodyAst, acc = go acc body
      let _, acc = go acc parenR
      bodyAst, acc

    | Syn.Abs (lambda, ident, dot, colon, ty, body) ->
      let _, acc = go acc lambda

      let ident, acc =
        match ident with
        | Syn.Token (TokenKind.Ident, (l, r)) ->
          let ident = text |> strSlice l r
          Some ident, acc
        | _ ->
          let _, acc = go acc ident
          None, acc

      let _, acc = go acc colon
      let ty, acc = go acc ty
      let _, acc = go acc dot

      let bodyAst, acc = go acc body

      let ast =
        match ident, ty, bodyAst with
        | Some ident, Some ty, Some bodyAst ->
          Some (Ast.Abs (ident, ty, bodyAst))
        | _ ->
          None

      ast, acc

    | Syn.App (cal, arg) ->
      let calAst, acc = go acc cal
      let argAst, acc = go acc arg

      let ast =
        match calAst, argAst with
        | Some calAst, Some argAst ->
          Some (Ast.App (calAst, argAst))
        | _ ->
          None

      ast, acc

    | Syn.Semi (first, semi, second) ->
      let firstAst, acc = go acc first
      let _, acc = go acc semi
      let secondAst, acc = go acc second

      let ast =
        match firstAst, secondAst with
        | Some firstAst, Some secondAst ->
          Some (Ast.Semi (firstAst, secondAst))
        | _ ->
          None

      ast, acc

  let ast, errorAcc = go [] syn
  let errors = errorAcc |> List.rev

  text, ast, errors
