namespace DynCalc.Lang

open DynCalc.Lang.Derive.Derive
open DynCalc.Lang.FP.Ast

module Ast =
    type Symbol<'IdentifierSymbol> = 'IdentifierSymbol

    type Args<'BuiltinArg> = 'BuiltinArg list

    type AstType =
        | AstNumber
        | AstNumberList
        | AstT of string


    type BuiltinExternalCall<'BuiltinCall, 'BuiltinArg> = FunctionCall of 'BuiltinCall * Args<'BuiltinArg>

    type ParserType = AstType * DeriveParser option

    type AssignmentContent<'RefSymbol, 'BuiltinCall, 'BuiltinArg, 'TypeSymbol> =
        | FunctionAssignment of Function
        // A1 = 2 * 3 + 5
        | ExprAssignment of Expression
        // A2 <- ws("ws://...") as IntWsFinnHub
        | ProviderAssignment of BuiltinExternalCall<'BuiltinCall, 'BuiltinArg> * AstType option
        // A3 -> http()
        | SinkAssignment of BuiltinExternalCall<'BuiltinCall, 'BuiltinArg>
        // The type parser gives the value 7296.89
        // IntWsFinnHub :: number deriving (json "$.data[-1].p")
        | TypeAssignment of ParserType

    type Assignment<'RefSymbol, 'BuiltinCall, 'BuiltinArg, 'TypeSymbol> =
        | Assignment of Symbol<'RefSymbol> * AssignmentContent<'RefSymbol, 'BuiltinCall, 'BuiltinArg, 'TypeSymbol>


    let exprToString ref =
        function
        | Object x -> $"{ref} = {objectToString x};"
        | Application (id, x) -> $"{id}:{objectToString x}"

    let providerToString ref (FunctionCall (name, args)) typ =
        $"""{ref} <- {name.ToString()}({String.concat ","
                                        <| List.map (fun x -> $"\"{x}\"") args}) {(Option.map (fun x -> " as " + x.ToString()) typ
                                                                                   |> Option.defaultValue "")}"""

    let sinkToString ref (FunctionCall (name, args)) =

        $"""{ref.ToString()} -> {name.ToString()}({String.concat ","
                                                   <| List.map (fun x -> $"\"{x}\"") args})"""

    let typedefToString symbol typ deriver =
        $"""{symbol.ToString()} :: {typ.ToString()} deriving egg"""

    let toStringContent (var, assignment) =
        match assignment with
        | ExprAssignment e -> exprToString var e
        | ProviderAssignment (e, t) -> providerToString var e t
        | SinkAssignment e -> sinkToString var e
        | TypeAssignment (typ, deriver) -> typedefToString var typ deriver
        | FunctionAssignment (_) -> failwith "Not Implemented functionassignment"

    let toString =
        function
        | Assignment (v, e) -> toStringContent (v, e)

// using annotated AST
/// Implemented by http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf
module Inference =
    open Ast
    open FSharpx.Collections

    type TType =
        | TNumber
        | TNumberList
        | TT of string
        | TApplication of TType * TType


    type Scheme = Scheme of List<string> * TType

    type Substitution = Map<string, TType>

    type TypeEnv = Map<string, Scheme>

    exception TypeError of string * TType * TType

    type Types<'a> =
        { FreeTypeVariable: 'a -> Set<string>
          Apply: Substitution -> 'a -> 'a }

    let rec typesTypeImpl: Types<TType> =
        { FreeTypeVariable =
            fun t ->
                match t with
                | TT n -> Set.singleton n
                | TApplication (t1, t2) ->
                    Set.union (typesTypeImpl.FreeTypeVariable t1) (typesTypeImpl.FreeTypeVariable t2)
                | TNumber
                | TNumberList -> Set.empty
          Apply =
            fun s t ->
                match t with
                | TT n -> Map.tryFind n s |> Option.defaultValue t
                | TApplication (t1, t2) -> TApplication(typesTypeImpl.Apply s t1, typesTypeImpl.Apply s t2)
                | TNumber
                | TNumberList -> t }

    let rec typesSchemeImpl: Types<Scheme> =
        { FreeTypeVariable =
            fun (Scheme (vars, t)) -> Set.difference (typesTypeImpl.FreeTypeVariable t) (Set.ofList vars)

          Apply = fun s (Scheme (vars, t)) -> Scheme(vars, typesTypeImpl.Apply(List.foldBack Map.remove vars s) t) }

    let typesListImpl (types: Types<'a>) : Types<List<'a>> =
        { FreeTypeVariable = fun l -> List.foldBack Set.union (List.map types.FreeTypeVariable l) Set.empty

          Apply = fun s l -> List.map (types.Apply s) l }

    let typesTypeEnvImpl: Types<TypeEnv> =
        { FreeTypeVariable =
            fun env ->
                (typesListImpl typesSchemeImpl)
                    .FreeTypeVariable(List.map snd (Map.toList env))
          Apply = fun s env -> Map.map (fun _ scheme -> typesSchemeImpl.Apply s scheme) env }

    let composeSubst (s1: Substitution) (s2: Substitution) : Substitution =
        let ns2 = Map.map (fun _k (v: TType) -> typesTypeImpl.Apply s1 v) s2

        Map.union ns2 s1

    /// Abstracts a type over all type variables that are not free
    let generalize (env: TypeEnv) (t: TType) : Scheme =
        Scheme(
            List.ofSeq (
                typesTypeImpl.FreeTypeVariable t
                - typesTypeEnvImpl.FreeTypeVariable env
            ),
            t
        )

    let newTypeVar =
        let mutable nextIndex = 1

        fun n ->
            let nn = sprintf "%s%d" n nextIndex
            nextIndex <- nextIndex + 1
            TT nn

    /// Replace all bound type variables with fresh variables
    let instantiate (Scheme (variables, t): Scheme) =
        let nvars = List.map (fun _ -> newTypeVar "a") variables

        let s = Map.ofSeq (Seq.zip variables nvars)
        typesTypeImpl.Apply s t

    /// Unified to most general unifier
    let rec unify (t1: TType) (t2: TType) : Substitution =
        match t1, t2 with
        | TT u, t -> varBind u t
        | t, TT u -> varBind u t
        | TApplication (t1, t2), TApplication (t1', t2') ->
            let s1 = unify t1 t1'

            let s2 = unify (typesTypeImpl.Apply s1 t2) (typesTypeImpl.Apply s1 t2')

            composeSubst s1 s2

        | TNumber, TNumber -> Map.empty
        | TNumberList, TNumberList -> Map.empty
        | _ -> failwith $"Types do not unify: {t1} vs {t2}"

    and varBind (u: string) (t: TType) : Substitution =
        match t with
        | TT u' when u = u' -> Map.empty
        | _ when (typesTypeImpl.FreeTypeVariable t).Contains u -> failwith $"Occur check failed: {u} vs {t}"
        | _ -> Map.ofList [ (u, t) ]


    let nullSubstitution: Substitution = Map.empty

    let mathOperatorDef = (TApplication(TNumber, (TApplication(TNumber, TNumber))))

// let rec inferMathExpr (env: TypeEnv) (e: MathExpr<_, _, _>) =
//     match e with
//     | Variable n ->
//         match env.TryFind n with
//         | None -> failwith $"Unbound variable: {n}"
//         | Some sigma ->
//             let t = instantiate sigma
//             nullSubstitution, t
//     | Number _ -> (nullSubstitution, TNumber)
//     | Binary (e1, _, e2) ->
//         let (s1, t1) = inferMathExpr env e1
//         let (s2, t2) = inferMathExpr env e2
//         let tv = newTypeVar "b"
//         let s3 = unify mathOperatorDef (TApplication(t1, (TApplication(t2, tv))))
//         let composed = composeSubst s1 (composeSubst s2 s3)
//         (composed, typesTypeImpl.Apply s3 tv)

// let translateAstToT typ =
//     match typ with
//     | AstNumber -> TNumber
//     | AstNumberList -> TNumberList
//     | AstT t -> TT t

// let inferAssignment (env: TypeEnv) (Assignment (symbol, a)) =
//     match a with
//     | ExprAssignment (me) ->
//         let (s1, t1) = inferMathExpr env me
//         let tp = generalize (typesTypeEnvImpl.Apply s1 env) t1

//         Map.add symbol tp env

//     | TypeAssignment (typ, _) ->
//         let typ = translateAstToT typ
//         let tp = generalize env typ
//         Map.add symbol tp env

//     | ProviderAssignment (_, optTyp) ->
//         // TODO: unify with inferred function type. Same for sink
//         Option.map
//             (fun typ ->
//                 let typ = translateAstToT typ
//                 let tp = generalize env typ
//                 Map.add symbol tp env)
//             optTyp
//         |> Option.defaultValue env


//     | SinkAssignment (_) -> env

// let typeInference (env: Map<string, Scheme>) assignments =
//     List.fold (inferAssignment) env assignments
