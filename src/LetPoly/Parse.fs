/// Syntactical analysis.
module rec LetPoly.Parse

open LetPoly.Types
open LetPoly.Helpers

// -----------------------------------------------
// Helpers
// -----------------------------------------------

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
    Syn.Error ("Expected an identifier", nextRange tokens, None), tokens

let eatDot tokens =
  match tokens with
  | (TokenKind.Dot, _) as token :: tokens ->
    Syn.Token token, tokens
  | _ ->
    Syn.Error ("Expected '.'", nextRange tokens, None), tokens

let eatParenR leftParenRange tokens =
  match tokens with
  | (TokenKind.ParenR, _) as token :: tokens ->
    Syn.Token token, tokens
  | _ ->
    let syn = Syn.Error ("Not closed by ')'", leftParenRange, None)
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
    let semi = Syn.Error ("Expected ';'", nextRange tokens, None)
    semi, tokens

// -----------------------------------------------
// Parse non-terminals
// -----------------------------------------------

let parseAtom tokens =
  match tokens with
  | (TokenKind.IntLit, _) as token :: tokens ->
    Syn.Node (noId, SynKind.IntLit, [Syn.Token  token]), tokens

  | (TokenKind.Ident, _) as token :: tokens ->
    Syn.Node (noId, SynKind.Var, [Syn.Token token]), tokens

  | (TokenKind.ParenL, range) as parenL :: tokens ->
    let term, tokens = parseTerm tokens
    let parenR, tokens = eatParenR range tokens
    Syn.Node (noId, SynKind.Paren, [Syn.Token parenL; term; parenR]), tokens

  | (TokenKind.Lambda, _) as lambda :: tokens ->
    let ident, tokens = eatIdent tokens
    let dot, tokens = eatDot tokens
    let body, tokens = parseTerm tokens
    Syn.Node (noId, SynKind.Abs, [Syn.Token lambda; ident; dot; body]), tokens

  | _ ->
    Syn.Error ("Expected a term", nextRange tokens, None), tokens

/// `app = atom+`
let parseApp tokens =
  let rec go cal tokens =
    match tokens with
    // `first(term) \ first(abs)`
    | (TokenKind.IntLit, _) :: _
    | (TokenKind.Ident, _) :: _
    | (TokenKind.ParenL, _) :: _ ->
      let arg, tokens = parseAtom tokens
      let cal = Syn.Node (noId, SynKind.App, [cal; arg])
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
    Syn.Error ("Unexpected token", range, Some term), tokens

  | [] ->
    Syn.Error ("Expected a term but EOF", noRange, None), tokens

/// `semi = term (';'+ term?)*`
let parseSemi tokens =
  let rec go first tokens =
    let semi, tokens = eatSemi tokens
    match tokens with
    | [] ->
      first, []
    | _ ->
      let second, tokens = parseTerm tokens
      let first = Syn.Node (noId, SynKind.Semi, [first; semi; second])
      go first tokens

  let term, tokens = parseTerm tokens
  go term tokens

let parseEof syn tokens =
  match tokens with
  | [] ->
    syn
  | _ ->
    Syn.Error ("Expected EOF", noRange, Some syn)

let parseRoot tokens =
  let _, tokens = eatSemi tokens
  let syn, tokens = parseSemi tokens
  parseEof syn tokens

let parse (text: string, tokens: Token list): string * Syn =
  text, parseRoot tokens
