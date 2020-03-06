// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "C:/Users/Bruger/Documents/DTU/Datalogisk modellering/packages/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
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
    | Num(x) -> true
    | Var(x) -> true
    | TimesExpr(x,y) -> evalExpr(x) && evalExpr (y)
    | DivExpr(x,y) -> evalExpr(x) && evalExpr (y)
    | PlusExpr(x,y) -> evalExpr(x) && evalExpr (y)
    | MinusExpr(x,y) -> evalExpr(x) && evalExpr (y)
    | PowExpr(x,y) -> evalExpr(x) && evalExpr (y)
    | UPlusExpr(x) -> evalExpr(x)
    | UMinusExpr(x) -> evalExpr(x)
    | Index(x) -> evalExpr(x)
    | _ -> false
and evalLogic b =
    match b with
    | True(x) -> true
    | False(x) -> false
    | NotLogic(x) -> evalLogic(x)
    | AndSCLogic(x,y) -> evalLogic(x) && evalLogic(y)
    | AndLogic(x,y) -> evalLogic(x) && evalLogic(y)
    | OrLogic(x,y) -> evalLogic(x) && evalLogic(y)
    | OrSCLogic(x,y) -> evalLogic(x) && evalLogic(y)
    | EqualLogic(x,y) -> evalExpr(x) && evalExpr(y) 
    | NotEqualLogic(x,y) -> evalExpr(x) && evalExpr(y) 
    | GTLogic(x,y) -> evalExpr(x) && evalExpr(y) 
    | GETLogic(x,y) -> evalExpr(x) && evalExpr(y) 
    | LTLogic(x,y) -> evalExpr(x) && evalExpr(y) 
    | LETLogic(x,y) -> evalExpr(x) && evalExpr(y)  
    | _ -> false



 //   match b with
 //   | True(x) -> true
 //   | False(x) -> false
 //   | NotLogic(x) -> not(evalLogic(x))
 //   | AndSCLogic(x,y) -> if evalLogic(x) then evalLogic(y) else false
 //   | AndLogic(x,y) -> if evalLogic(x) then (if evalLogic(y) then true else false) else false
 //   | OrLogic(x,y) -> if evalLogic(x) then true else evalLogic(y)
 //   | OrSCLogic(x,y) -> evalLogic(x) || evalLogic(y)
 //   | EqualLogic(x,y) -> evalExpr(x) = evalExpr(y) 
 //   | NotEqualLogic(x,y) -> evalExpr(x) <> evalExpr(y) 
 //   | GTLogic(x,y) -> evalExpr(x) > evalExpr(y) 
 //   | GETLogic(x,y) -> evalExpr(x) >= evalExpr(y) 
 //   | LTLogic(x,y) -> evalExpr(x) < evalExpr(y) 
 //   | LETLogic(x,y) -> evalExpr(x) <= evalExpr(y) 
 //   | _ -> false

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
        printfn "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        //printfn "Result: %b" (evalExpr(e))
        printfn "Result: %b" (evalLogic(e))
        compute n
        with err -> printfn "wrong syntax"
                    compute (n-1)

// Start interacting with the user
compute 3
