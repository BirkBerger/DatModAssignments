// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll" // Thea
// #r "C:/Users/Bruger/Documents/DTU/Datalogisk modellering/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll" // Vivian
// "C:/Users/amali/source/repos/DataMod/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll" // Amalie
open FSharp.Text.Lexing
open System
#load "ParserTypesAST.fs"
open ParserTypesAST
#load "ParserParser.fs"
open ParserParser
#load "ParserLexer.fs"
open ParserLexer



// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec evalExpr e =
  match e with
    | Num(x)                    -> true
    | Var(x)                    -> true
    | TimesExpr(x,y)
    | DivExpr(x,y)
    | PlusExpr(x,y)
    | MinusExpr(x,y)
    | PowExpr(x,y)              -> evalExpr(x) && evalExpr (y)
    | UPlusExpr(x)              -> evalExpr(x)
    | UMinusExpr(x)             -> evalExpr(x)
    | Index(x)                  -> evalExpr(x)
and evalLogic b =
    match b with
    | True(x)                   -> true
    | False(x)                  -> true
    | NotLogic(x)               -> evalLogic(x)
    | AndSCLogic(x,y)
    | AndLogic(x,y)
    | OrLogic(x,y)
    | OrSCLogic(x,y)            -> evalLogic(x) && evalLogic(y)
    | EqualLogic(x,y)
    | NotEqualLogic(x,y)
    | GTLogic(x,y)
    | GETLogic(x,y)
    | LTLogic(x,y)
    | LETLogic(x,y)             -> evalExpr(x) && evalExpr(y)
and evalIdent i =
    match i with
    | Identification(x)         -> true
and evalCmd c =
    match c with
    | Assign(x,y)               -> evalIdent(x) && evalExpr(y)
    | ArrAssign(x, y, z)        -> evalExpr(y) && evalExpr(z)
    | Skip                      -> true
    | CmdSequence(x,y)          -> evalCmd(x) && evalCmd(y)
    | If(x)                     -> evalGrdCmd(x)
    | Do(x)                     -> evalGrdCmd(x)
and evalGrdCmd g =
    match g with
    | Then(x,y)                 -> evalLogic(x) && evalCmd(y)
    | GrdCmdSequence(x,y)       -> evalGrdCmd(x) && evalGrdCmd(y)

    



// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = ParserParser.start ParserLexer.tokenize lexbuf
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
        // printfn "Result: %b" (evalExpr(e))
        printfn "Syntax correct - according to GCL grammar." 
        compute n
        with err -> printfn "Syntax invalid - according to GCL grammar."
                    compute (n-1)

// Start interacting with the user
compute 3
