// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module InterpreterTypesAST


// Arithmetic expressions
type expr =
  | Num of int
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | Var of string
  | Index of (string*expr)
  | ParenExpr of (expr)

// Boolean expressions
type logic =
  | TrueLogic
  | FalseLogic
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
  | ParenLogic of (logic)

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

// Edge labels
type label = 
  | StringLabel of string
  | LogicLabel of logic
  | CmdLabel of cmd


//
type init =
  | VarInit of string * int
  | ArrInit of string * arrElem
  | SeqInit of init * init
and arrElem =
  | NumElem of int
  | ElemSeq of int * arrElem


type mapKey = 
  | VarElem of string
  | ArrElem of string * int
  
type memory = Map<mapKey,int>

type AST =
  | CommandND of cmd
  | CommandD of cmd*string
  | GuardedND of grdCmd
  | GuardedD of grdCmd*string

type numberOfSteps =
  | Integer of int


exception ProgramStuckError of string * memory
exception ProgramNotFinishedError of string * memory