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
  /// `(`
  | ParenL
  /// `)`
  | ParenR
  | Dot
  /// `\` (λ)
  | Lambda
  /// `:`
  | Colon
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
  | TyVar
    of ident:Syn
  | Ty
    of Syn
  /// 42
  | IntLit
    of intLit:Syn
  /// x
  | Var
    of ident:Syn
  /// (t)
  | Paren
    of parenL:Syn * body:Syn * parenR:Syn
  /// \x:X. t
  | Abs
    of lambda:Syn * ident:Syn * dot:Syn * colon:Syn * ty:Syn * body:Syn
  /// (callee, argument)
  /// f t
  | App
    of cal:Syn * arg:Syn
  /// t; t
  | Semi
    of first:Syn * semi:Syn * second:Syn

/// Node of abstract syntax tree.
type Ast =
  | TyVar
    of string
  | IntLit
    of int
  | Var
    of string
  | Abs
    of ident: string * ty: Ast * body: Ast
  | App
    of cal: Ast * arg:Ast
  | Semi
    of Ast * Ast

/// Node of abstract syntax tree.
/// Variables are de Bruijn indices.
type IndexAst =
  | TyVar
    of int
  | IntLit
    of int
  | Var
    of DeBruijnIndex * ContextLength
  | Abs
    of ident: Name * ty: IndexAst * body: IndexAst
  | App
    of cal: IndexAst * arg: IndexAst
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
  | IntLit
    of TermId * int
  | Var
    of TermId * DeBruijnIndex * ContextLength
  | Abs
    of TermId * Name * Term
  | App
    of TermId * Term * Term

type Ty =
  | Con
    of int

type Command =
  | Eval
    of Term
