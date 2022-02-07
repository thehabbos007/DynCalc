namespace DynCalc.Core.Runtime

open DynCalc.Lang.FP.Ast
open DynCalc.Lang.FP.Builtins
open DynCalc.Lang.FP.Interpreter
open DynCalc.Lang.Derive.Derive
open DynCalc.Lang.Ast
open DynCalc.Lang.Parser
open DynCalc.Lang.MultilineParser
open FSharp.Data.Adaptive
open DynCalc.Core.Runtime.Graph
open DynCalc.Core.Providers
open DynCalc.Core.Providers.Mapper
open DynCalc.Core.Sinks
open DynCalc.Core.Sinks.Mapper

type Context<'a, 'b when 'a: comparison> =
    { Env: Environment
      Graph: Graph<Symbol<'a>>
      Inputs: Map<Symbol<'a>, (ProviderSchemas * cval<'b> * ParserType)>
      Intermediates: Map<Symbol<'a>, aval<'b>>
      Outputs: List<Symbol<'a> * SinkSchemas * aval<'b>>
      ParseTypes: Map<Symbol<'a>, ParserType> }

module Evaluator =

    let internal updateMap key value =
        Map.change key (fun opt ->
            Some
            <| match opt with
               | Some x -> x
               | None -> value)

    let internal maybeUpdateMap key value map =
        value
        |> Option.map (fun value -> updateMap key value map)
        |> Option.defaultValue map

    let internal aconst = AVal.constant
    let internal avalify x = x :> aval<_>

    let adaptiveList (values: list<aval<'a>>) : aval<list<'a>> =
        AVal.custom (fun token -> values |> List.map (fun c -> c.GetValue token))

    let interpretExprA (ctx: Context<_, _>) (variable: Symbol<_>) (expr: Expression) =
        let rec flattenAdaptive o =
            match o with
            | DynamicVariable dvar ->
                match Map.tryFind (dvar) ctx.Intermediates with
                | Some adaptive -> adaptive
                | None -> failwith $"Error! could not find {dvar} from {variable}"
            | Bottom -> AVal.constant (Bottom)
            | AtomObject (NumberAtom c) -> AVal.constant <| AtomObject(NumberAtom c)
            | AtomObject (BoolAtom c) -> AVal.constant <| AtomObject(BoolAtom c)
            | AtomObject (SymbolAtom c) -> AVal.constant <| AtomObject(SymbolAtom c)
            | SequenceObject os -> AVal.map (SequenceObject) (adaptiveList (List.map (flattenAdaptive) os))

        let newExpr =
            match expr with
            | Object o -> AVal.map (Object) (flattenAdaptive o)
            | Application (f, o) -> AVal.map (fun o -> Application(f, o)) (flattenAdaptive o)

        let res =
            AVal.map
                (fun e ->
                    match interpretExpr ctx.Env e with
                    | Object o -> o
                    | _ -> failwith "Evaluated function resulted in application..")
                newExpr

        (ctx, Some res)

    let interpretProviderAssignment (ctx: Context<_, _>) variable expr typ =
        let parserType =
            match typ with
            | Some (AstT tName) ->
                Map.tryFind tName ctx.ParseTypes
                |> Option.defaultWith (fun () -> failwith $"Undefined type {tName}, {ctx}")
            | Some (t) -> (t, None)
            | _ -> (AstNumber, None)

        let (schema, changable) = mapProviderSchema expr

        ({ ctx with Inputs = updateMap variable (schema, changable, parserType) ctx.Inputs }, Some(avalify (changable)))

    let interpretSinkAssignment (ctx: Context<_, _>) variable expr =
        let aval =
            match Map.tryFind (variable) ctx.Intermediates with
            | Some adaptive -> adaptive
            | None -> failwith $"Error! could not find '{variable}' in environment {ctx.Intermediates}"

        ({ ctx with
            Outputs =
                (variable, (mapSinkSchema expr), aval)
                :: ctx.Outputs },
         None)

    let interpretAssignment (ctx: Context<_, _>) (Assignment (variable, expr)) =
        match expr with
        | TypeAssignment t -> ({ ctx with ParseTypes = updateMap variable t ctx.ParseTypes }, None)
        | FunctionAssignment func -> ({ ctx with Env = addToEnv variable (UserDefined func) ctx.Env }, None)
        | ExprAssignment expr -> interpretExprA ctx variable expr
        | ProviderAssignment (expr, typeName) -> interpretProviderAssignment ctx variable expr typeName
        | SinkAssignment expr -> interpretSinkAssignment ctx variable expr

    let evaluate programAssignment (ctx: Context<string, _>) idx =
        let var = fst ctx.Graph.[idx]
        let expr = Map.find var programAssignment
        let (ctx, exprval) = interpretAssignment ctx expr

        { ctx with Intermediates = maybeUpdateMap var exprval ctx.Intermediates }


    let ctxWithGraph g =
        { Env = builtinEnv
          Graph = g
          Inputs = Map.empty
          Intermediates = Map.empty
          Outputs = List.empty
          ParseTypes = Map.empty }

    let evaluateG (program: Program<string, _, _>) =
        let (typesAndFunctions, assignments) =
            List.partition
                (function
                | Assignment (_, TypeAssignment _)
                | Assignment (_, FunctionAssignment _) -> true
                | _ -> false)
                program

        let (sinks, assignments) =
            List.partition
                (function
                | Assignment (_, SinkAssignment _) -> true
                | _ -> false)
                assignments

        let g = graphFromProgram assignments
        let order = toposortThrows g

        let assignments =
            assignments
            |> List.map (fun (Assignment (variable, _) as expr) -> (variable, expr))
            |> Map.ofList

        let ctx = ctxWithGraph g

        let ctx =
            List.fold (fun ctx x -> interpretAssignment ctx x |> fst) ctx typesAndFunctions

        let ctx = List.fold (evaluate assignments) ctx order
        List.fold (fun ctx x -> interpretAssignment ctx x |> fst) ctx sinks
