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

// ------------------ Task1: GLC parser ------------------ //

// checkSyntaxExpr: Evaluates the correctness of the syntax of the input guarded commands code
let rec checkSyntaxExpr e =
  match e with
    | Num(x)                        -> true
    | Var(x)                        -> true
    | TimesExpr(x,y)                -> checkSyntaxExpr(x) && checkSyntaxExpr (y)
    | DivExpr(x,y)                  -> checkSyntaxExpr(x) && checkSyntaxExpr (y)
    | PlusExpr(x,y)                 -> checkSyntaxExpr(x) && checkSyntaxExpr (y)
    | MinusExpr(x,y)                -> checkSyntaxExpr(x) && checkSyntaxExpr (y)
    | PowExpr(x,y)                  -> checkSyntaxExpr(x) && checkSyntaxExpr (y)
    | UPlusExpr(x)                  -> checkSyntaxExpr(x)
    | UMinusExpr(x)                 -> checkSyntaxExpr(x)
    | Index(array,index)            -> checkSyntaxExpr(index)
    | ParenExpr(x)                  -> checkSyntaxExpr(x)
and checkSyntaxLogic b =
    match b with
    | TrueLogic                     -> true
    | FalseLogic                    -> true
    | NotLogic(x)                   -> checkSyntaxLogic(x)
    | AndSCLogic(x,y)               -> checkSyntaxLogic(x) && checkSyntaxLogic(y)
    | AndLogic(x,y)                 -> checkSyntaxLogic(x) && checkSyntaxLogic(y)
    | OrLogic(x,y)                  -> checkSyntaxLogic(x) && checkSyntaxLogic(y)
    | OrSCLogic(x,y)                -> checkSyntaxLogic(x) && checkSyntaxLogic(y)
    | EqualLogic(x,y)               -> checkSyntaxExpr(x) && checkSyntaxExpr(y) 
    | NotEqualLogic(x,y)            -> checkSyntaxExpr(x) && checkSyntaxExpr(y) 
    | GTLogic(x,y)                  -> checkSyntaxExpr(x) && checkSyntaxExpr(y) 
    | GETLogic(x,y)                 -> checkSyntaxExpr(x) && checkSyntaxExpr(y) 
    | LTLogic(x,y)                  -> checkSyntaxExpr(x) && checkSyntaxExpr(y) 
    | LETLogic(x,y)                 -> checkSyntaxExpr(x) && checkSyntaxExpr(y) 
    | ParenLogic(x)                 -> checkSyntaxLogic(x)
and checkSyntaxCmd c =
    match c with
    | AssignVar(id,exp)             -> checkSyntaxExpr(exp)
    | AssignArr(array, index, exp)  -> checkSyntaxExpr(index) && checkSyntaxExpr(exp)
    | Skip                          -> true
    | SeqCmd(cmd1, cmd2)            -> checkSyntaxCmd(cmd1) && checkSyntaxCmd(cmd2)
    | IfCmd(grdCmd)                 -> checkSyntaxGrdCmd(grdCmd)
    | DoCmd(grdCmd)                 -> checkSyntaxGrdCmd(grdCmd)
and checkSyntaxGrdCmd gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> checkSyntaxLogic(bool) && checkSyntaxCmd(cmd)
    | SeqGrdCmd(gc1,gc2)            -> checkSyntaxGrdCmd(gc1) && checkSyntaxGrdCmd(gc2)


// ------------------ Task2: GLC compiler ------------------ //

// expToString: Converts the input arithmetic expression to string fit for edge labels
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

// logicToString: Converts the input boolean expression to string fit for edge labels
let rec logicToString b =
    match b with
    | TrueLogic                     -> "true"
    | FalseLogic                    -> "false"
    | NotLogic(x)                   -> "!" + x.ToString()
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

// doneGrdCmdL: Outputs the label belonging to the edge expressing the non-satisfied input guarded command
let rec doneGrdCmd gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> "!(" + (logicToString bool) + ")"
    | SeqGrdCmd(gc1,gc2)            -> "(" + (doneGrdCmd gc1) + "&&" + (doneGrdCmd gc2) + ")"

// edgesCmd: Converts the input command to a list of edges of the corresponding deterministic program graph 
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

// edgesD: Converts the input command to a list of edges of the corresponding non-deterministic program graph 
let rec edgesD q1 q2 qAcc c =
    match c with
    | AssignVar(var,exp)            -> [(q1, var + ":=" + (expToString exp), q2)]
    | AssignArr(array,index,exp)    -> [(q1, array + "[" + (expToString index) + "]:=" + (expToString exp), q2)]
    | Skip                          -> [(q1, "skip", q2)]
    | SeqCmd(cmd1,cmd2)             -> let qFresh = qAcc + 1
                                       (edgesD q1 qFresh (qAcc+1) cmd1)@(edgesD qFresh q2 (qAcc+1) cmd2)
    | IfCmd(gc)                     -> let (e,d) = edgesD2 q1 q2 qAcc gc "false"
                                       e
    | DoCmd(gc)                     -> let (e,d) = edgesD2 q1 q1 qAcc gc "false"
                                       e@[(q1, "!(" + d + ")", q2)]
and edgesD2 q1 q2 qAcc gc d =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1 
                                       ([(q1, "(" + (logicToString bool) + ")&(!" + d + ")", qFresh)]@(edgesD qFresh q2 (qAcc+1) cmd), "(" + (logicToString bool) + ")" + "|" + d)
    | SeqGrdCmd(gc1, gc2)           -> let (e1,d1) = edgesD2 q1 q2 qAcc gc1 d
                                       let (e2,d2) = edgesD2 q1 q2 qAcc gc2 d1
                                       (e1@e2,d2)




let graphvizIntro = "digraph program_graph {rankdir=LR;
                        node [shape = circle]; q▷;
                        node [shape = doublecircle]; q◀;
                        node [shape = circle]\n"

// stateToString: Takes a state integer and outputs its corresponding state name
let stateToString = function
    | 0         -> "q▷"
    | -1        -> "q◀"
    | q         -> "q" + q.ToString()

// printProgramTree: Ouputs program tree in graphviz format of a given edge list
let rec printProgramTree eList =
    match eList with
    | []                             -> ""
    | (q1,label,q2)::es              -> (stateToString q1) + " -> " + (stateToString q2) + " [label = \"" + label + "\"];\n" + (printProgramTree es)



// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CompilerParser.start CompilerLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// promtGraphType: Promts the user for graph type until either "D", "ND", or "E" is input
let rec promtGraphType input = 
    match input with
    | "D" | "ND" | "E"      -> input
    | _                     -> printfn "\nEnter the following to chose program graph type:\nD - for deterministic, or \nND - for non-deterministic\nEnter E to exit.\n"
                               promtGraphType (Console.ReadLine())

// We implement here the function that interacts with the user
let rec compute n gType =
    if n = 0 then
        printfn "Bye bye"
    else
        let graphType = if (gType = "") then promtGraphType gType else gType
        if graphType = "E" then (compute 0 "")
        else 
            try
            printfn "Enter a command: "
            let e = parse (Console.ReadLine())

            if (graphType = "D") then printfn "Program graph:\n %s%s}" graphvizIntro (printProgramTree (edgesCmd 0 -1 0 e))
                                 else printfn "Program graph:\n %s%s}" graphvizIntro (printProgramTree (edgesD 0 -1 0 e))
            compute n ""

            with err -> printfn "Invalid syntax according to GLC grammar"
                        compute (n-1) graphType
                        

// Start interacting with the user
compute 3 ""
