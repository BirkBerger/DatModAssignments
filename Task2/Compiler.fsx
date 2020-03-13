// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
// #r "C:/Users/Bruger/Documents/DTU/Datalogisk modellering/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
// #r "C:/Users/amali/source/repos/DataMod/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "CompilerTypesAST.fs"
open CompilerTypesAST
#load "CompilerParser.fs"
open CompilerParser
#load "CompilerLexer.fs"
open CompilerLexer



// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec evalExpr e =
  match e with
    | Num(x)                        -> true
    | Var(x)                        -> true
    | TimesExpr(x,y)                -> evalExpr(x) && evalExpr (y)
    | DivExpr(x,y)                  -> evalExpr(x) && evalExpr (y)
    | PlusExpr(x,y)                 -> evalExpr(x) && evalExpr (y)
    | MinusExpr(x,y)                -> evalExpr(x) && evalExpr (y)
    | PowExpr(x,y)                  -> evalExpr(x) && evalExpr (y)
    | UPlusExpr(x)                  -> evalExpr(x)
    | UMinusExpr(x)                 -> evalExpr(x)
    | Index(array,index)            -> evalExpr(index)
    | ParenExpr(x)                  -> evalExpr(x)
and evalLogic b =
    match b with
    | True(x)                       -> true
    | False(x)                      -> true
    | NotLogic(x)                   -> evalLogic(x)
    | AndSCLogic(x,y)               -> evalLogic(x) && evalLogic(y)
    | AndLogic(x,y)                 -> evalLogic(x) && evalLogic(y)
    | OrLogic(x,y)                  -> evalLogic(x) && evalLogic(y)
    | OrSCLogic(x,y)                -> evalLogic(x) && evalLogic(y)
    | EqualLogic(x,y)               -> evalExpr(x) && evalExpr(y) 
    | NotEqualLogic(x,y)            -> evalExpr(x) && evalExpr(y) 
    | GTLogic(x,y)                  -> evalExpr(x) && evalExpr(y) 
    | GETLogic(x,y)                 -> evalExpr(x) && evalExpr(y) 
    | LTLogic(x,y)                  -> evalExpr(x) && evalExpr(y) 
    | LETLogic(x,y)                 -> evalExpr(x) && evalExpr(y) 
    | ParenLogic(x)                 -> evalLogic(x)
and evalCmd c =
    match c with
    | AssignVar(id,exp)             -> evalExpr(exp)
    | AssignArr(array, index, exp)  -> evalExpr(index) && evalExpr(exp)
    | Skip                          -> true
    | SeqCmd(cmd1, cmd2)            -> evalCmd(cmd1) && evalCmd(cmd2)
    | IfCmd(grdCmd)                 -> evalGrdCmd(grdCmd)
    | DoCmd(grdCmd)                 -> evalGrdCmd(grdCmd)
and evalGrdCmd gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> evalLogic(bool) && evalCmd(cmd)
    | SeqGrdCmd(gc1,gc2)            -> evalGrdCmd(gc1) && evalGrdCmd(gc2)

let rec expToString e =
    match e with
    | Num(x)                        -> x.ToString()
    | Var(x)                        -> x.ToString()
    | TimesExpr(x,y)                -> (expToString x) + "*" + (expToString y)
    | DivExpr(x,y)                  -> (expToString x) + "/" + (expToString y)
    | PlusExpr(x,y)                 -> (expToString x) + "+" + (expToString y)
    | MinusExpr(x,y)                -> (expToString x) + "-" + (expToString y)
    | PowExpr(x,y)                  -> (expToString x) + "^" + (expToString y)
    | UPlusExpr(x)                  -> "+" + (expToString x)
    | UMinusExpr(x)                 -> "-" + (expToString x)
    | Index(array,index)            -> array + "[" + index.ToString() + "]"
    | ParenExpr(x)                 -> "(" + (expToString x) + ")"

let rec logicToString b =
    match b with
    | True(x)                       -> "true"
    | False(x)                      -> "false"
    | NotLogic(x)                   -> "not " + x.ToString()
    | AndSCLogic(x,y)               -> (logicToString x) + "&&" + (logicToString y)
    | AndLogic(x,y)                 -> (logicToString x) + "&" + (logicToString y)
    | OrLogic(x,y)                  -> (logicToString x) + "|" + (logicToString y)
    | OrSCLogic(x,y)                -> (logicToString x) + "||" + (logicToString y)
    | EqualLogic(x,y)               -> (expToString x) + "=" + (expToString y)
    | NotEqualLogic(x,y)            -> (expToString x) + "!=" + (expToString y)
    | GTLogic(x,y)                  -> (expToString x) + ">" + (expToString y)
    | GETLogic(x,y)                 -> (expToString x) + ">=" + (expToString y)
    | LTLogic(x,y)                  -> (expToString x) + "<" + (expToString y)
    | LETLogic(x,y)                 -> (expToString x) + "<=" + (expToString y)
    | ParenLogic(x)                 -> "(" + (logicToString x) + ")"

let stateToString = function
    | 1000 -> "qEnd"
    | q     -> "q" + q.ToString()

let rec doneGrdCmd gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> "!(" + (logicToString bool)  + ")"
    | SeqGrdCmd(gc1,gc2)            -> (doneGrdCmd gc1) + "&&" + (doneGrdCmd gc2)

let rec edgesCmd q1 q2 qAcc c =
    match c with
    | AssignVar(var,exp)            -> [(q1, var + ":=" + (expToString exp), q2)]
    | AssignArr(array,index,exp)    -> [(q1, array + "[" + (expToString index) + "]:=" + (expToString exp), q2)]
    | Skip                          -> [(q1, "skip", q2)]
    | SeqCmd(cmd1,cmd2)             -> let qFresh = qAcc + 1
                                       (edgesCmd q1 qFresh (qAcc+1) cmd1)@(edgesCmd qFresh q2 (qAcc+1) cmd2)
    | IfCmd(grdCmd)                 -> edgesGrdCmd q1 q2 qAcc grdCmd
    | DoCmd(grdCmd)                 -> let b = doneGrdCmd grdCmd
                                       (edgesGrdCmd q1 q1 qAcc grdCmd)@[(q1,b,q2)]
and edgesGrdCmd q1 q2 qAcc gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1
                                       [(q1,(logicToString bool),qFresh)]@(edgesCmd qFresh q2 (qAcc+1) cmd)
    | SeqGrdCmd(gc1, gc2)           -> (edgesGrdCmd q1 q2 qAcc gc1)@(edgesGrdCmd q1 q2 qAcc gc2)

let rec printProgramTree eList =
    match eList with
    | []                             -> ""
    | (q1,label,q2)::es              -> (stateToString q1) + " -> " + (stateToString q2) + " [label = " + label + "];\n" + (printProgramTree es)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CompilerParser.start CompilerLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Enter a command: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %A" e 
        printfn "Program graph:\n%A" (printProgramTree (edgesCmd 0 1000 0 e))
        compute n
        with err -> printfn "Invalid syntax according to GLC grammar"
                    compute (n-1)

// Start interacting with the user
compute 3
