/ This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
// #r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
//#r "C:/Users/Bruger/Documents/DTU/Datalogisk modellering/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
 #r "C:/Users/amali/source/repos/DataMod/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "InterpreterTypesAST.fs"
open InterpreterTypesAST
#load "InterpreterParser.fs"
open InterpreterParser
#load "InterpreterLexer.fs"
open InterpreterLexer

// ------------------ Task1: GLC parser ------------------ //

// checkSyntaxExpr: Evaluates the correctness of the syntax of the input guarded commands code
let rec checkSyntaxExpr e =
  match e with
    | Num(x)                        -> x
    | Var(x)                        -> x
    | TimesExpr(x,y)                -> checkSyntaxExpr(x) * checkSyntaxExpr (y)
    | DivExpr(x,y)                  -> checkSyntaxExpr(x) / checkSyntaxExpr (y)
    | PlusExpr(x,y)                 -> checkSyntaxExpr(x) + checkSyntaxExpr (y)
    | MinusExpr(x,y)                -> checkSyntaxExpr(x) - checkSyntaxExpr (y)
    | PowExpr(x,y)                  -> checkSyntaxExpr(x) ** checkSyntaxExpr (y)
    | UPlusExpr(x)                  -> + checkSyntaxExpr(x)
    | UMinusExpr(x)                 -> - checkSyntaxExpr(x)
    | Index(array,index)            -> checkSyntaxExpr(array[index])
    | ParenExpr(x)                  -> checkSyntaxExpr(x)
and checkSyntaxLogic b =
    match b with
    | TrueLogic                     -> true
    | FalseLogic                    -> true
    | NotLogic(x)                   -> checkSyntaxLogic(x)
    | AndSCLogic(x,y)               -> checkSyntaxLogic(x) && checkSyntaxLogic(y)
    | AndLogic(x,y)                 -> (checkSyntaxLogic(x) && checkSyntaxLogic(y)) && (checkSyntaxLogic(x) && checkSyntaxLogic(y))
    | OrLogic(x,y)                  -> (checkSyntaxLogic(x) || checkSyntaxLogic(y)) && (checkSyntaxLogic(x) || checkSyntaxLogic(y))
    | OrSCLogic(x,y)                -> checkSyntaxLogic(x) || checkSyntaxLogic(y)
    | EqualLogic(x,y)               -> checkSyntaxExpr(x) = checkSyntaxExpr(y) 
    | NotEqualLogic(x,y)            -> checkSyntaxExpr(x) <> checkSyntaxExpr(y) 
    | GTLogic(x,y)                  -> checkSyntaxExpr(x) > checkSyntaxExpr(y) 
    | GETLogic(x,y)                 -> checkSyntaxExpr(x) >= checkSyntaxExpr(y) 
    | LTLogic(x,y)                  -> checkSyntaxExpr(x) < checkSyntaxExpr(y) 
    | LETLogic(x,y)                 -> checkSyntaxExpr(x) <= checkSyntaxExpr(y) 
    | ParenLogic(x)                 -> checkSyntaxLogic(x)
and checkSyntaxCmd c =
    match c with
    | AssignVar(id,exp)             -> checkSyntaxExpr(exp)
    | AssignArr(array, index, exp)  -> array[checkSyntaxExpr(index)] = checkSyntaxExpr(exp)
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
    | AssignVar(var,exp)            -> [(q1, Command(c), q2)]
    | AssignArr(array,index,exp)    -> [(q1, Command(c), q2)]
    | Skip                          -> [(q1, Command(c), q2)]
    | SeqCmd(cmd1,cmd2)             -> let qFresh = qAcc + 1
                                       (edgesCmd q1 qFresh (qAcc+1) cmd1)@(edgesCmd qFresh q2 (qAcc+1) cmd2)
    | IfCmd(grdCmd)                 -> edgesGrdCmd q1 q2 qAcc grdCmd
    | DoCmd(grdCmd)                 -> (edgesGrdCmd q1 q1 qAcc grdCmd)@[(q1,Command(c),q2)]
and edgesGrdCmd q1 q2 qAcc gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1
                                       [(q1,GuardedND(gc),qFresh)]@(edgesCmd qFresh q2 (qAcc+1) cmd)
    | SeqGrdCmd(gc1, gc2)           -> (edgesGrdCmd q1 q2 qAcc gc1)@(edgesGrdCmd q1 q2 qAcc gc2)

// edgesD: Converts the input command to a list of edges of the corresponding non-deterministic program graph 
let rec edgesD q1 q2 qAcc c =
    match c with
    | AssignVar(var,exp)            -> [(q1, Command(c), q2)]
    | AssignArr(array,index,exp)    -> [(q1, Command(c), q2)]
    | Skip                          -> [(q1, Command(c), q2)]
    | SeqCmd(cmd1,cmd2)             -> let qFresh = qAcc + 1
                                       (edgesD q1 qFresh (qAcc+1) cmd1)@(edgesD qFresh q2 (qAcc+1) cmd2)
    | IfCmd(gc)                     -> let (e,d) = edgesD2 q1 q2 qAcc gc "false"
                                       e
    | DoCmd(gc)                     -> let (e,d) = edgesD2 q1 q1 qAcc gc "false"
                                       e@[(q1, CommandD(c,d), q2)]
and edgesD2 q1 q2 qAcc gc d =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1 
                                       ([(q1, GuardedD(gc,d), qFresh)]@(edgesD qFresh q2 (qAcc+1) cmd), "(" + (logicToString bool) + ")" + "|" + d)
    | SeqGrdCmd(gc1, gc2)           -> let (e1,d1) = edgesD2 q1 q2 qAcc gc1 d
                                       let (e2,d2) = edgesD2 q1 q2 qAcc gc2 d1
                                       (e1@e2,d2)




let graphvizNotations = "digraph program_graph {rankdir=LR;
                        node [shape = circle]; q▷;
                        node [shape = doublecircle]; q◀;
                        node [shape = circle]\n";;

// stateToString: Takes a state integer and outputs its corresponding state name
let stateToString = function
    | 0 -> "q▷"
    | 1000 -> "q◀"
    | q     -> "q" + q.ToString()

//printLabel: Outputs the labels to the edges in the programtree
let printLabel(label) =
    match label with
    |Command(x)            ->     match x with
                                    | AssignVar(var,exp)            ->  var + ":=" + (expToString exp)
                                    | AssignArr(array,index,exp)    -> array + "[" + (expToString index) + "]:=" + (expToString exp)
                                    | Skip                          -> "skip"
                                    | DoCmd(grdCmd)                 -> doneGrdCmd grdCmd
    | GuardedND(x)         ->     match  x with
                                    | ThenGrdCmd(bool,cmd)          -> logicToString(bool)
    | CommandD(x,s)        ->     match  (x,s) with
                                    | (DoCmd(gc),d)                 -> "!(" + d + ")"
    | GuardedD(x,s)        ->     match (x,s) with
                                    | (ThenGrdCmd(bool,cmd),d)      -> "(" + (logicToString bool) + ")&(!" + d + ")"

// printProgramTree: Ouputs program tree in graphviz format of a given edge list
let rec printProgramTree eList =
    match eList with
    | []                             -> ""
    | (q1,label,q2)::es              -> (stateToString q1) + " -> " + (stateToString q2) + " [label = \"" + printLabel(label) + "\"];\n" + (printProgramTree es)


(* let rec printVariables variables =
    match variables with
    |[]              -> ""
    |(name, num)::vs -> name + ": " + num + "\n" + printVariables vs
let printStatus q status variables = 
    "status: " + status + "\n Node: " + (string) q + "\n" + (printVariables variables)
let interpreter q eListFull eList variables =
    match eList with
    | []                when q=1000  ->  printStatus q "terminated" variables
    | []                             ->  printStatus q "stuck" variables
    | (q1,label,q2)::es  when q1==q  ->  Calculate(label)
                                         interpreter q2 eListFull eListFull variables
    | (_,_,_)::es                    ->  interpreter q eListFull es variables ;; *)





// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = InterpreterParser.start InterpreterLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// promtGraphType: Promts the user for graph type until either "D", "ND", or "E" is input
let rec promtGraphType = function
    | "D"           -> "D"
    | "ND"          -> "ND"
    | "E"           -> "E"
    | _             -> printfn "\nEnter the following to chose program graph type:\nD - for deterministic, or \nND - for non-deterministic\nEnter E to exit.\n"
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

            if (graphType = "D") then let programtree = edgesCmd 0 1000 0 e
                                      printfn "Program graph:\n %s%s}" graphvizNotations (printProgramTree (programtree))
                                 //   interpreter q0 programtree programtree variables
                                 else let programtree = edgesD 0 1000 0 e
                                      printfn "Program graph:\n %s%s}" graphvizNotations (printProgramTree (programtree))
                                      //   interpreter q0 programtree programtree variables
            compute n ""

            with err -> printfn "Invalid syntax according to GLC grammar"
                        compute (n-1) graphType
                        

// Start interacting with the user
compute 3 ""