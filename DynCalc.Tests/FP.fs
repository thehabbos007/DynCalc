module FpTests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.GenBuilder

open DynCalc.Lang.FP.Ast
open DynCalc.Lang.FP.Builtins
open DynCalc.Lang.FP.Interpreter

let func =
    gen {
        let! (item, _) = Gen.elements builtinEnvList
        return (Function item)
    }

type Builtin =
    static member Function() : Arbitrary<Function> = func |> Arb.fromGen

    static member Functions() : Arbitrary<List<Function>> = Gen.nonEmptyListOf func |> Arb.fromGen

    static member Constant() =
        gen {
            let! num =
                Arb.generate<float>
                |> Gen.filter (fun f ->
                    not <| System.Double.IsNaN(f)
                    && not <| System.Double.IsInfinity(f))

            return numberfy num
        }
        |> Arb.fromGen

let testFn = interpretApplication builtinEnv

[<assembly: Properties(Arbitrary = [| typeof<Builtin> |])>]
do ()

module LawsOfAlgebra =

    [<Property>]
    let ``[f,g].h = [f.h,g.h]`` o f g h =

        testFn (Composition(FunctionList([ f; g ]), h)) o = testFn
                                                                (FunctionList([ Composition(f, h); Composition(g, h) ]))
                                                                o
    // Property helped find issue with `trans` implementation
    [<Property>]
    let ``[f1,...,fn].g = [f1.g,...,fn.g]`` o fns g =
        testFn (Composition(FunctionList fns, g)) o = testFn (FunctionList(List.map (fun f -> Composition(f, g)) fns)) o

    // Property found issue with non bottom-preserving
    [<Property>]
    let ``forall f . [g1,...,gn] = [f.g1,...,f.gn]`` o f gns =
        testFn (Composition((ApplyToAll f), FunctionList gns)) o = testFn
                                                                       (FunctionList(
                                                                           List.map (fun g -> Composition(f, g)) gns
                                                                       ))
                                                                       o

    [<Property>]
    let ``f.[#x,g] = (bu f x).g`` o (num: ObjectGeneric<float>) f g =

        testFn (Composition(f, FunctionList([ Constant num; g ]))) o = testFn (Composition(BinaryToUnary(f, num), g)) o
