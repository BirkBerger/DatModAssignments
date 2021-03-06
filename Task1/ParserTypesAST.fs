// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module ParserTypesAST


// Arithmetic expressions
type expr =
  | Num of float
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | Var of string
  | Index of (string*expr)

// Boolean expressions
type logic =
  | True of bool
  | False of bool
  | NotLogic of (logic)
  | AndLogic of (logic*logic)
  | AndSCLogic of (logic*logic)
  | OrLogic of (logic*logic)
  | OrSCLogic of (logic*logic)
  | EqualLogic of (expr*expr)
  | NotEqualLogic of (expr*expr)
  | GTLogic of (expr*expr)
  | GETLogic of (expr*expr)
  | LTLogic of (expr*expr)
  | LETLogic of (expr*expr)

// Commands
type cmd =
  | AssignVar of (string*expr)
  | AssignArr of (string*expr*expr)
  | Skip
  | SeqCmd of (cmd*cmd)
  | IfCmd of (grdCmd)
  | DoCmd of (grdCmd)
// Guarded commands
and grdCmd =
  | ThenGrdCmd of (logic*cmd)
  | SeqGrdCmd of (grdCmd*grdCmd)