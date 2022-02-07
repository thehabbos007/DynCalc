namespace DynCalc.Lang

open FParsec
//open FSharp.Core
open DynCalc.Lang.Ast
open DynCalc.Lang.Parser

module MultilineParser =
    type Program<'RefSymbol, 'BuiltinCall, 'BuiltinArg> =
        (Assignment<'RefSymbol, 'BuiltinCall, 'BuiltinArg, AstType>) list

    type ParseResult<'RefSymbol, 'BuiltinCall, 'BuiltinArg> =
        | Good of Program<'RefSymbol, 'BuiltinCall, 'BuiltinArg>
        | Bad of string

    let programParse = run (many (spaces >>. parseLine .>> spaces))

    let unwrapParseProgram x : Program<string, string, string> =
        match programParse x with
        | Success (x, _, _) -> x
        | Failure (x, _, _) -> failwithf "Failed to parse, %s" x

    let parseMaybe x : Program<_, _, _> option =
        match programParse x with
        | Success (x, _, _) -> Some x
        | _ -> None

    let parseResult x =
        match programParse x with
        | Success (x, _, _) -> Good x
        | Failure (_, x, _) -> Bad(x.ToString())
