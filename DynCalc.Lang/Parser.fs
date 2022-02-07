namespace DynCalc.Lang

open DynCalc.Lang.Ast
open DynCalc.Lang.Derive.Derive
open FParsec
open DynCalc.Lang.FP.Ast
open DynCalc.Lang.FP.Parser

module Parser =
    let str_ws str = skipString str >>. spaces

    let typeRegex = regex @"[A-Z][A-Za-z0-9]*"

    let betweenSame a x = between a a x
    let wsIgnore x = betweenSame spaces x

    let wrapped l r x =
        wsIgnore x |> between (pchar l) (pchar r)

    // "standard" JSON string literal parser.
    // https://github.com/stephan-tolksdorf/fparsec/blob/master/Samples/JSON/parser.fs#L31
    let stringLiteral =
        let escape =
            anyOf "\"\\/bfnrt"
            |>> function
                | 'b' -> "\b"
                | 'f' -> "\u000C"
                | 'n' -> "\n"
                | 'r' -> "\r"
                | 't' -> "\t"
                | c -> string c

        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

            pchar 'u'
            >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3) * 4096
                + (hex2int h2) * 256
                + (hex2int h1) * 16
                + hex2int h0
                |> char
                |> string)

        let escaped = pchar '\\' >>. (escape <|> unicodeEscape)

        let anyChar = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (pchar '\"') (pchar '\"') (stringsSepBy anyChar escaped)

    let args =
        wrapped '(' ')'
        <| sepBy stringLiteral (wsIgnore (pstring ","))

    let semicolon = spaces .>> pchar ';'

    let mathAssignment =
        pchar '=' >>. spaces >>. expression
        |>> ExprAssignment


    let functionCall = variableIdentifier .>>. args |>> FunctionCall


    let builtinType =
        (stringReturn "Number[]" AstNumberList)
        <|> stringReturn "Number" AstNumber

    let typeParser: Parser<AstType, unit> = builtinType <|> (typeRegex |>> AstT)

    let providerAssignment =
        pstring "<-" >>. spaces >>. functionCall
        .>> spaces
        .>>. (opt (pstring "as" >>. spaces >>. typeParser))
        |>> ProviderAssignment

    let sinkAssignment =
        pstring "->" >>. wsIgnore functionCall
        |>> SinkAssignment


    let deriver =
        wsIgnore (pstring "json") >>. stringLiteral
        .>> spaces
        |>> Json

    let typeAssignment =
        pstring "type" >>. spaces >>. typeRegex
        .>> (wsIgnore (pstring "::"))
        .>>. builtinType
        .>>. wsIgnore (
            (pstring "deriving"
             >>. wsIgnore (wrapped '(' ')' deriver))
        )
        |>> fun ((sym, typ), deriver) -> Assignment(sym, TypeAssignment(typ, Some deriver))

    let functionAssignment = definition

    let parseLine: Parser<Assignment<string, string, string, AstType>, unit> =
        ((functionAssignment
          |>> fun (Definition (var, f)) -> Assignment(var, FunctionAssignment f))
         <|> typeAssignment
         <|> (variableIdentifier .>> spaces
              .>>. (mathAssignment
                    <|> providerAssignment
                    <|> sinkAssignment)
              |>> Assignment))
        .>> spaces
        .>> semicolon



    let exprParse = run parseLine

    let parseMaybe x =
        match exprParse x with
        | Success (x, _, _) -> Some x
        | _ -> None

    let unwrapParse x =
        match exprParse x with
        | Success (x, _, _) -> x
        | Failure (x, _, _) -> failwithf "Failed to parse, %s" x
