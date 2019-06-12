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
  | Ident
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
// Parse
// -----------------------------------------------

type SynError =
  | SynErr
    of string * TextRange

/// Node of concrete syntax tree.
type Syn =
  | Error
    of string * TextRange * Syn list
  | Token
    of Token
  /// x
  | Var
    of ident:Syn
  /// (t)
  | Paren
    of parenL:Syn * body:Syn * parenR:Syn
  /// \x. t
  | Abs
    of lambda:Syn * ident:Syn * dot:Syn * body:Syn
  /// (callee, argument)
  /// f t
  | App
    of cal:Syn * arg:Syn
  /// t; t
  | Semi
    of first:Syn * semi:Syn * second:Syn

/// Node of abstract syntax tree.
type Ast =
  | Var
    of string
  | Abs
    of string * Ast
  | App
    of Ast * Ast
  | Semi
    of Ast * Ast

/// Node of abstract syntax tree.
/// Variables are de Bruijn indices.
type IndexAst =
  | Var
    of DeBruijnIndex * ContextLength
  | Abs
    of Name * IndexAst
  | App
    of IndexAst * IndexAst
  | Semi
    of IndexAst * IndexAst

// -----------------------------------------------
// Terms
// -----------------------------------------------

type TermId = int

type DeBruijnIndex = int

type ContextLength = int

type Name = string

type NameContext = string list

type TermData =
  | Known
    of pos:int
  | Unknown

type Term =
  | Var
    of TermId * DeBruijnIndex * ContextLength
  | Abs
    of TermId * Name * Term
  | App
    of TermId * Term * Term

type Command =
  | Eval
    of Term
