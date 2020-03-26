// Implementation file for parser generated by fsyacc
module InitializerParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "InitializerParser.fsp"

open InterpreterTypesAST

# 10 "InitializerParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | LBRA2
  | RBRA2
  | EQUAL2
  | SEPAR
  | EOF
  | VAR2 of (string)
  | NUM2 of (float)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_LBRA2
    | TOKEN_RBRA2
    | TOKEN_EQUAL2
    | TOKEN_SEPAR
    | TOKEN_EOF
    | TOKEN_VAR2
    | TOKEN_NUM2
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_arrayElem
    | NONTERM_initialize

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | LBRA2  -> 0 
  | RBRA2  -> 1 
  | EQUAL2  -> 2 
  | SEPAR  -> 3 
  | EOF  -> 4 
  | VAR2 _ -> 5 
  | NUM2 _ -> 6 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_LBRA2 
  | 1 -> TOKEN_RBRA2 
  | 2 -> TOKEN_EQUAL2 
  | 3 -> TOKEN_SEPAR 
  | 4 -> TOKEN_EOF 
  | 5 -> TOKEN_VAR2 
  | 6 -> TOKEN_NUM2 
  | 9 -> TOKEN_end_of_input
  | 7 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_arrayElem 
    | 3 -> NONTERM_arrayElem 
    | 4 -> NONTERM_initialize 
    | 5 -> NONTERM_initialize 
    | 6 -> NONTERM_initialize 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 9 
let _fsyacc_tagOfErrorTerminal = 7

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | LBRA2  -> "LBRA2" 
  | RBRA2  -> "RBRA2" 
  | EQUAL2  -> "EQUAL2" 
  | SEPAR  -> "SEPAR" 
  | EOF  -> "EOF" 
  | VAR2 _ -> "VAR2" 
  | NUM2 _ -> "NUM2" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | LBRA2  -> (null : System.Object) 
  | RBRA2  -> (null : System.Object) 
  | EQUAL2  -> (null : System.Object) 
  | SEPAR  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | VAR2 _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM2 _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 5us; 6us; 10us; 11us; 2us; 65535us; 0us; 2us; 14us; 13us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 6us; 1us; 1us; 2us; 2us; 3us; 1us; 3us; 1us; 3us; 2us; 4us; 5us; 2us; 4us; 5us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 5us; 2us; 6us; 6us; 1us; 6us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 12us; 14us; 16us; 19us; 22us; 24us; 26us; 28us; 30us; 33us; |]
let _fsyacc_action_rows = 15
let _fsyacc_actionTableElements = [|1us; 32768us; 5us; 7us; 0us; 49152us; 2us; 32768us; 3us; 14us; 4us; 3us; 0us; 16385us; 1us; 16386us; 3us; 5us; 1us; 32768us; 6us; 4us; 0us; 16387us; 1us; 32768us; 2us; 8us; 2us; 32768us; 0us; 10us; 6us; 9us; 0us; 16388us; 1us; 32768us; 6us; 4us; 1us; 32768us; 1us; 12us; 0us; 16389us; 1us; 16390us; 3us; 14us; 1us; 32768us; 5us; 7us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 6us; 7us; 9us; 11us; 12us; 14us; 17us; 18us; 20us; 22us; 23us; 25us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 3us; 3us; 5us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 3us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 16387us; 65535us; 65535us; 16388us; 65535us; 65535us; 16389us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 110 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : init)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 119 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : init)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "InitializerParser.fsp"
                                                         _1 
                   )
# 34 "InitializerParser.fsp"
                 : init));
# 130 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "InitializerParser.fsp"
                                         NumElem(_1) 
                   )
# 44 "InitializerParser.fsp"
                 : arrElem));
# 141 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arrElem)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "InitializerParser.fsp"
                                                      ElemSeq(_1,_3) 
                   )
# 45 "InitializerParser.fsp"
                 : arrElem));
# 153 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "InitializerParser.fsp"
                                                  VarInit(_1,_3) 
                   )
# 48 "InitializerParser.fsp"
                 : init));
# 165 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : arrElem)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "InitializerParser.fsp"
                                                                ArrInit(_1,_4) 
                   )
# 49 "InitializerParser.fsp"
                 : init));
# 177 "InitializerParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : init)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : init)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "InitializerParser.fsp"
                                                          SeqInit(_1,_3) 
                   )
# 50 "InitializerParser.fsp"
                 : init));
|]
# 190 "InitializerParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 10;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : init =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
