namespace DynCalc.Core.Runtime

open DynCalc.Lang.Ast
open DynCalc.Lang.Parser
open DynCalc.Lang.FP.Ast
open DynCalc.Lang.MultilineParser

type Vertex = int
type Graph<'a> = ('a * Vertex list) array

type Edge = Vertex * Vertex

module Graph =

    let vertices (g: Graph<'a>) : Vertex list = List.init g.Length id

    let edges (g: Graph<'a>) : Edge list =
        vertices g
        |> List.collect (fun v -> snd g.[v] |> List.map (fun v2 -> v, v2))

    /// Function is partial in that it does _not_ traverse the whole graph.
    let dfs (graph: Graph<'a>) alreadyVisited start =
        let rec explore path visitedResult node =
            Result.bind
                (fun visited ->
                    if Set.contains node path then
                        Error $"A cycle has been identified at {node}"
                    else if List.contains node visited then
                        Ok visited
                    else
                        let path = Set.add node path
                        let edges = snd graph.[node]

                        let visited = List.fold (explore path) (Ok(visited)) edges

                        Result.bind (fun visited -> Ok(node :: visited)) visited)
                visitedResult

        explore (Set.empty) alreadyVisited start

    let toposort graph =
        List.fold (fun visited node -> dfs graph visited node) (Ok([])) (vertices graph)

    let dfsThrows (graph: Graph<'a>) alreadyVisited start =
        let rec explore path visited node =
            if Set.contains node path then
                failwith $"A cycle has been identified at {node}"
            else if List.contains node visited then
                visited
            else
                let path = Set.add node path
                let edges = snd graph.[node]
                let visited = List.fold (explore path) visited edges
                node :: visited

        explore (Set.empty) alreadyVisited start

    let toposortThrows graph =
        List.fold (fun visited node -> dfsThrows graph visited node) [] (vertices graph)


    // Graph construction follows

    let internal uncurry f (a, b) = f a b

    let internal orElse def opt =
        match opt with
        | Some _ -> opt
        | None -> Some(def)


    let rec findReferenceNameExpr assignment expr =
        match expr with
        | Bottom -> []
        | AtomObject _ -> []
        | DynamicVariable var -> [ (var, assignment) ]
        | SequenceObject objs -> List.collect (findReferenceNameExpr assignment) objs

    // Fetch all dependants from AST.
    // get back [(a, b)] where b depends on a
    let rec referencesFromAst program =
        List.collect (findReferenceName) program

    and findReferenceName (Assignment (var, content)) =
        match content with
        | ExprAssignment expr ->
            let obj =
                match expr with
                | Object o
                | Application (_, o) -> o

            findReferenceNameExpr var obj
        | _ -> []


    // Using the dependencies from previously, construct a map containing non-dependencies
    let dependencies refs program =
        // Initial dependency list _without_ non-dependencies
        let deps =
            List.groupBy (fst) refs
            |> List.map (fun (key, value) -> (key, (List.map (snd) value)))
        // Augmented dependency lists _with_ non-dependencies
        let deps =
            List.fold
                (fun map (Assignment (variable, _)) -> Map.change variable (orElse []) map)
                (deps |> Map.ofList)
                program
            |> Map.toList

        let numbered =
            List.mapi (fun i (key, _) -> (key, i)) deps
            |> Map.ofList

        (deps, numbered)

    // construct a Graph from a program
    let graphFromProgram (program: Program<_, _, _>) : Graph<Symbol<string>> =
        let refs = referencesFromAst program

        let (deps, numbered) = dependencies refs program

        // From the numbered map, we can lookup the array index of the Location
        // This is used to translate locations _in the dependency list_ to array indices
        // which results in efficient DAG construction.
        let translated =
            List.map (fun (key, values) -> (key, List.map (fun v -> Map.find v numbered) values)) deps
            |> List.toArray

        translated
