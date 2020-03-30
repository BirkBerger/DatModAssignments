// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"                                                                   // Thea
// #r "C:/Users/Bruger/Documents/DTU/Datalogisk modellering/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"  // Vivian
// #r "C:/Users/amali/source/repos/DataMod/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"                   // Amalie
open FSharp.Text.Lexing
open System
// CG language parser and lexer
#load "InterpreterTypesAST.fs"
open InterpreterTypesAST
#load "InterpreterParser.fs"
open InterpreterParser
#load "InterpreterLexer.fs"
open InterpreterLexer
// Initializer language parser and lexer
#load "InitializerParser.fs"
open InitializerParser
#load "InitializerLexer.fs"
open InitializerLexer



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
    | AssignVar(var,exp)            -> [(q1, CommandND(c), q2)]
    | AssignArr(array,index,exp)    -> [(q1, CommandND(c), q2)]
    | Skip                          -> [(q1, CommandND(c), q2)]
    | SeqCmd(cmd1,cmd2)             -> let qFresh = qAcc + 1
                                       (edgesCmd q1 qFresh (qAcc+1) cmd1)@(edgesCmd qFresh q2 (qAcc+1) cmd2)
    | IfCmd(grdCmd)                 -> edgesGrdCmd q1 q2 qAcc grdCmd
    | DoCmd(grdCmd)                 -> (edgesGrdCmd q1 q1 qAcc grdCmd)@[(q1,CommandND(c),q2)]
and edgesGrdCmd q1 q2 qAcc gc =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1
                                       [(q1,GuardedND(gc),qFresh)]@(edgesCmd qFresh q2 (qAcc+1) cmd)
    | SeqGrdCmd(gc1, gc2)           -> (edgesGrdCmd q1 q2 qAcc gc1)@(edgesGrdCmd q1 q2 qAcc gc2)

// edgesD: Converts the input command to a list of edges of the corresponding non-deterministic program graph 
let rec edgesD q1 q2 qAcc c =
    match c with
    | AssignVar(var,exp)            -> [(q1, CommandND(c), q2)]
    | AssignArr(array,index,exp)    -> [(q1, CommandND(c), q2)]
    | Skip                          -> [(q1, CommandND(c), q2)]
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
    | 0         -> "q▷"
    | -1        -> "q◀"
    | q         -> "q" + q.ToString()

//printLabel: Outputs the labels to the edges in the programtree
let printLabel(label) =
    match label with
    | CommandND(x)      ->  match x with
                                | AssignVar(var,exp)            ->  var + ":=" + (expToString exp)
                                | AssignArr(array,index,exp)    -> array + "[" + (expToString index) + "]:=" + (expToString exp)
                                | Skip                          -> "skip"
                                | DoCmd(grdCmd)                 -> doneGrdCmd grdCmd
                                | _                             -> failwith "command cannot be label"
    | GuardedND(x)      ->  match  x with
                                | ThenGrdCmd(bool,cmd)          -> logicToString(bool)
                                | _                             -> failwith "guarded command cannot be label"
    | CommandD(x,s)     ->  match  (x,s) with
                                | (DoCmd(gc),d)                 -> "!(" + d + ")"
                                | _                             -> failwith "command cannot be label"
    | GuardedD(x,s)     ->  match (x,s) with
                                | (ThenGrdCmd(bool,cmd),d)      -> "(" + (logicToString bool) + ")&(!" + d + ")"
                                | _                             -> failwith "guarded command cannot be label"

// printProgramTree: Ouputs program tree in graphviz format of a given edge list
let rec printProgramTree eList =
    match eList with
    | []                             -> ""
    | (q1,label,q2)::es              -> (stateToString q1) + " -> " + (stateToString q2) + " [label = \"" + printLabel(label) + "\"];\n" + (printProgramTree es)




// ------------------ Task3: GLC interpreter ------------------ //

let rec power2 x y = 
    match y with
    | 0             -> x
    | _ when y > 0  -> power2 (x*x) (y-1)
    | _             -> failwith "cannot have negative exponent"

// power: Calculate x to the power of y
let power x y = if y=0 then 1 else power2 x y

// evalExpr: Evaluates arithmetic expression
let rec evalExpr state step e mem =
  match e with
    | _ when step = 0               -> raise (ProgramNotFinishedError("evaluation is not finished",mem,state))
    | Num(x)                        -> x
    | Var(x)                        -> if Map.containsKey (VarElem(x)) mem
                                       then Map.find (VarElem(x)) mem 
                                       else raise (ProgramStuckError("varible is not defined in memory",mem,state))
    | TimesExpr(x,y)                -> (evalExpr state step x mem) * (evalExpr state step y mem)
    | DivExpr(x,y)                  -> (evalExpr state step x mem) / (evalExpr state step y mem)
    | PlusExpr(x,y)                 -> (evalExpr state step x mem) + (evalExpr state step y mem)
    | MinusExpr(x,y)                -> (evalExpr state step x mem) - (evalExpr state step y mem)
    | PowExpr(x,y)                  -> power (evalExpr state step x mem) (evalExpr state step y mem)
    | UPlusExpr(x)                  -> (evalExpr state step x mem)
    | UMinusExpr(x)                 -> (-1) * (evalExpr state step x mem)
    | Index(name,index)             -> if Map.containsKey (ArrElem(name, evalExpr state step index mem)) mem
                                       then Map.find (ArrElem(name, evalExpr state step index mem)) mem 
                                       else raise (ProgramStuckError("array element is not defined in memory",mem,state))
    | ParenExpr(x)                  -> (evalExpr state step x mem)

// evalLogic: Evaluates boolean expression
let rec evalLogic state step b mem =
    match b with
    | _ when step = 0               -> raise (ProgramNotFinishedError("evaluation is not finished",mem,state))
    | TrueLogic                     -> true
    | FalseLogic                    -> false
    | NotLogic(x)                   -> evalLogic state step x mem
    | AndSCLogic(x,y)               -> (evalLogic state step x mem) && (evalLogic state step y mem)
    | AndLogic(x,y)                 -> ((evalLogic state step x mem) && (evalLogic state step y mem)) && ((evalLogic state step y mem) && (evalLogic state step x mem))
    | OrLogic(x,y)                  -> ((evalLogic state step x mem) || (evalLogic state step y mem)) && ((evalLogic state step y mem) || (evalLogic state step x mem))
    | OrSCLogic(x,y)                -> (evalLogic state step x mem) || (evalLogic state step y mem)
    | EqualLogic(x,y)               -> (evalExpr state step x mem) = (evalExpr state step y mem) 
    | NotEqualLogic(x,y)            -> (evalExpr state step x mem) <> (evalExpr state step y mem) 
    | GTLogic(x,y)                  -> (evalExpr state step x mem) > (evalExpr state step y mem) 
    | GETLogic(x,y)                 -> (evalExpr state step x mem) >= (evalExpr state step y mem) 
    | LTLogic(x,y)                  -> (evalExpr state step x mem) < (evalExpr state step y mem) 
    | LETLogic(x,y)                 -> (evalExpr state step x mem) <= (evalExpr state step y mem) 
    | ParenLogic(x)                 -> (evalLogic state step x mem)
  
// evalCmd: Evaluates command (deterministic)
let rec evalCmd q1 q2 qAcc step c mem =
    match c with
    | _ when step = 0               -> raise (ProgramNotFinishedError("evaluation is not finished",mem,q2))
    | AssignVar(id,exp)             -> let varValue = evalExpr q1 step exp mem
                                       if Map.containsKey (VarElem(id)) mem
                                       then Map.add (VarElem(id)) varValue (Map.remove (VarElem(id)) mem)
                                       else raise (ProgramStuckError("varible is not defined in memory",mem,q2))
    | AssignArr(name, index, exp)   -> let arrayIndex = evalExpr q1 step index mem
                                       let newArrayElem = evalExpr q1 step exp mem
                                       if Map.containsKey (ArrElem(name, arrayIndex)) mem
                                       then Map.add (ArrElem(name, arrayIndex)) newArrayElem (Map.remove (ArrElem(name, arrayIndex)) mem)
                                       else raise (ProgramStuckError("varible is not defined in memory",mem,q2))
    | Skip                          -> mem
    | SeqCmd(cmd1, cmd2)            -> let qFresh = qAcc + 1
                                       evalCmd q1 qFresh (qAcc+1) (step-2) cmd2 (evalCmd qFresh q2 (qAcc+1) (step-1) cmd1 mem)
    | IfCmd(grdCmd)                 -> evalGrdCmd q1 q2 qAcc (step-1) grdCmd mem
    | DoCmd(grdCmd)                 -> evalGrdCmd q1 q1 qAcc (step-1) grdCmd mem
// evalGrdCmd: Evaluates guarded command (deterministic)
and evalGrdCmd q1 q2 qAcc step gc mem =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1
                                       if (evalLogic q1 step bool mem) then (evalGrdCmd q1 q2 qAcc (step-1) gc (evalCmd qFresh q2 (qAcc+1) step cmd mem)) else mem
    | SeqGrdCmd(gc1,gc2)            -> evalGrdCmd q1 q2 qAcc step gc2 (evalGrdCmd q1 q2 qAcc step gc1 mem)


// evalCmdND: Evaluates command (non-deterministic)
let rec evalCmdND q1 q2 qAcc step c mem =
    match c with
    | _ when step = 0               -> raise (ProgramNotFinishedError("evaluation is not finished",mem,q2))
    | AssignVar(id,exp)             -> let varValue = evalExpr q1 step exp mem
                                       if Map.containsKey (VarElem(id)) mem
                                       then Map.add (VarElem(id)) varValue (Map.remove (VarElem(id)) mem)
                                       else raise (ProgramStuckError("varible is not defined in memory",mem,q2))
    | AssignArr(name,index,exp)     -> let arrayIndex = evalExpr q1 step index mem
                                       let newArrayElem = evalExpr q1 step exp mem
                                       if Map.containsKey (ArrElem(name, arrayIndex)) mem
                                       then Map.add (ArrElem(name, arrayIndex)) newArrayElem (Map.remove (ArrElem(name, arrayIndex)) mem)
                                       else raise (ProgramStuckError("varible is not defined in memory",mem,q2))
    | Skip                          -> mem
    | SeqCmd(cmd1,cmd2)             -> let qFresh = qAcc + 1
                                       evalCmd q1 qFresh (qAcc+1) (step-2) cmd2 (evalCmd qFresh q2 (qAcc+1) (step-1) cmd1 mem)
    | IfCmd(gc)                     -> evalGrdCmdND q1 q2 qAcc (step-1) gc mem
    | DoCmd(gc)                     -> evalGrdCmdND q1 q1 qAcc (step-1) gc mem
// evalGrdCmdND: Evaluates guarded command (non-deterministic)
and evalGrdCmdND q1 q2 qAcc step gc mem =
    match gc with
    | ThenGrdCmd(bool,cmd)          -> let qFresh = qAcc + 1 
                                       if (evalLogic q1 step bool mem) then (evalGrdCmdND q1 q2 qAcc (step-1) gc (evalCmdND qFresh q2 (qAcc+1) step cmd mem)) else mem
    | SeqGrdCmd(gc1, gc2)           -> evalGrdCmdND q1 q2 qAcc step gc2 (evalGrdCmdND q1 q2 qAcc step gc1 mem)
    


// printMemory: Prints memory list
let rec printMemory memList =
    match memList with
    | []                                              -> ""
    | (VarElem(x),value)::vs                          -> x + ": " + value.ToString() + "\n" + printMemory vs
    | (ArrElem(name, _),value)::vs                    -> name + ": [" + value.ToString() + (printArrayElements name vs)
and printArrayElements arrName memList =
    match memList with
    | (ArrElem(name, _),value)::vs when arrName=name  -> "," + value.ToString() + (printArrayElements arrName vs)
    | _                                               -> "]\n" + (printMemory memList)

// interpreter: Instantiates command evaluation and prints final state and memory
let rec interpreter command totalSteps mem graphType =
    try
        let finalMemoryD = evalCmd 0 -1 0 totalSteps command mem 
        let finalMemoryND = evalCmd 0 -1 0 totalSteps command mem 
        if (graphType = "D") then "\nStatus: Terminated\nNode: q◀\n" + (printMemory (Map.toList finalMemoryD))
                             else "\nStatus: Terminated\nNode: q◀\n" + (printMemory (Map.toList finalMemoryND))
    with
        | ProgramNotFinishedError(_,mem,q)                  -> "\nStatus: Program unfinished. Increment number of steps\nNode: " + (stateToString q) + "\n" + (printMemory (Map.toList mem))
        | ProgramStuckError(_,mem,q)                        -> "\nStatus: Stuck\nNode: " + (stateToString q) + "\n" +  (printMemory (Map.toList mem))

// getMemList: Create memory list from input initial variables and arrays
let rec getMemList a =
    match a with
    | VarInit(x,y)      -> [(VarElem(x),y)]
    | ArrInit(x,y)      -> (getArrMemList x y 0)
    | SeqInit(x,y)      -> (getMemList x)@(getMemList y)
// getArrMemList: Create memory element from every array element
and getArrMemList name mlist index =
    match mlist with
    | NumElem(x)        -> [(ArrElem(name,index),x)]
    | ElemSeq(x,xs)     -> [(ArrElem(name,index),x)]@(getArrMemList name xs (index+1))

let rec getMemMap a = Map.ofList (getMemList a)



// ------------------ Controller ------------------ //


// parser for command input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = InterpreterParser.start InterpreterLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
// parser for initialization of variables and arrays input
let parse2 input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = InitializerParser.start InitializerLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res





// strContainsOnlyNumber: Checks if input number is an integer
let strContainsOnlyNumber (s:string) = s |> Seq.forall Char.IsDigit




// inputNumberOfSteps: accepts number of steps from user
let rec inputNumberOfSteps cmd mem graphType n =
    if n = 0 then
        printfn "Bye bye"
    else 
        try  
            printfn "\nEnter number of steps"
            let numberOfSteps = Console.ReadLine()
            if (strContainsOnlyNumber numberOfSteps) && ((int) numberOfSteps) > 0 
            then printfn "%s" (interpreter cmd ((int) numberOfSteps) mem graphType)
            else failwith "wrong input"

            compute 3

        with err -> printfn "* Invalid number of steps *"
                    inputNumberOfSteps cmd mem graphType (n-1)

// inputInitialValues: accepts initialization settings from user
and inputInitialValues cmd graphType n = 
    if n = 0 then
        printfn "Bye bye"
    else 
        try
            printfn "\nEnter initial variables and array values"
            let initialMemory = parse2 (Console.ReadLine())
            let initialMemoryMap = getMemMap initialMemory
            
            inputNumberOfSteps cmd initialMemoryMap graphType 3

            with err -> printfn "* Invalid initialization of variables and arrays *"
                        inputInitialValues cmd graphType (n-1)
// inputGraphType: accepts graph type from user
and inputGraphType cmd n =
    if n = 0 then
        printfn "Bye bye"
    else 
        try
            let programtreeD = edgesCmd 0 -1 0 cmd
            let programtreeND = edgesD 0 -1 0 cmd

            printfn "\nEnter the following to chose program graph type:\nD - for deterministic, or \nND - for non-deterministic\n"
            let graphType = Console.ReadLine()

            if (graphType = "D") then printfn "Program graph:\n %s%s}" graphvizNotations (printProgramTree programtreeD)
            else if (graphType = "ND") then printfn "Program graph:\n %s%s}" graphvizNotations (printProgramTree programtreeND)
            else failwith "wrong input"

            inputInitialValues cmd graphType 3

        with err -> printfn "* Invalid graph type *"
                    inputGraphType cmd (n-1)
// compute: accept command input from user
and compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        try
            printfn "\nEnter a command: "
            let command = parse (Console.ReadLine())

            inputGraphType command 3

            with err -> printfn "* Invalid syntax according to GLC grammar *"
                        compute (n-1)
                        
                      
// Start interacting with the user
compute 3
