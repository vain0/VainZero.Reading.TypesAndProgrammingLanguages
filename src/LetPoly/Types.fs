[<AutoOpen>]
module rec LetPoly.Types

// -----------------------------------------------
// Tokens
// -----------------------------------------------

/// (start, end) in bytes
type TextRange = int * int

/// Kind of tokens.
type TokenKind =
  | Error
  | IntLit
  | Ident
  | True
  | False
  /// `(`
  | ParenL
  /// `)`
  | ParenR
  | Dot
  /// `\` (λ)
  | Lambda
  /// `;`
  | Semi

type Token = TokenKind * TextRange

// -----------------------------------------------
// Syntax trees
// -----------------------------------------------

type Serial = int

type SynId = int

type SynError =
  | SynError
    of string * TextRange

/// Node kind of concrete syntax tree.
type SynKind =
  | BoolLit
  | IntLit
  /// x
  | Var
  /// (t)
  | Paren
  /// \x. t
  | Abs
  /// (callee, argument)
  /// f t
  | App
  /// t; t
  | Semi

type Syn =
  | Error
    of msg: string * range: TextRange * Syn option
  | Missing
  | Token
    of Token
  | Node
    of SynId * SynKind * Syn list

// -----------------------------------------------
// HIR
// -----------------------------------------------

type TermId = int

type DeBruijnIndex = int

type ContextLength = int

type NameContext = string list

type Ty =
  | Meta
    of Serial
  | Any
  | Bool
  | Nat
  | Fun
    of Ty * Ty

type Term =
  | BoolLit
    of TermId * value: bool
  | IntLit
    of TermId * value: string
  | Var
    of TermId * name: string * DeBruijnIndex * ContextLength
  | Abs
    of TermId * name: string * ty: Ty * body: Term
  | App
    of TermId * cal: Term * arg: Term

type Command =
  | Eval
    of Term
