/// This module contains functions related to the `Simplex` and `Complex` types.
[<RequireQualifiedAccess>]
module Simplex

open Generics

/// The empty simplex.
let Empty = set [] |> Simplex

/// The empty complex.
let EmptyComplex = set [ Empty ] |> Complex

/// Computes the topological closure of a set of simplices.
let closure (sims : Set<Simplex>) : Complex =
    sims + set [ Empty ]
    |> Set.map (fun (Simplex s) -> Helpers.powerset s)
    |> Set.unionMany
    |> Set.map Simplex
    |> Complex

/// Convenience constructor to produce a simplicial complex from a partial specification of the simplices (eg. the toplexes).
let complex (input : List<List<int>>) : Complex =
    List.map (set >> ((fun x -> x |> Set.map Label) >> Simplex)) input
    |> set
    |> closure

/// Returns the top dimensional simplices of a complex.
let facets (Complex c) : Set<Simplex> =
    let nonToplexes =
        set [ for Simplex simplex in c do
                  for Simplex simplex2 in c do
                      if Set.isProperSubset simplex simplex2 then yield simplex ]
        |> Set.map Simplex
    c - nonToplexes

/// Topological boundary.
let boundary (com : Complex) =
    let simplexBoundary (Simplex sim) = sim |> Set.map (fun v -> sim - set [ v ] |> Simplex)
    com
    |> facets
    |> Set.map simplexBoundary
    |> Set.fold Helpers.symmetricDifference (set [ Empty ])
    |> closure

/// Computes the boundary matrix associated to two lists of simplices.
let boundaryMatrix (rows : List<Simplex>) (cols : List<Simplex>) : Matrix =
    let boundary r c = (rows.[r], cols.[c]) ||> fun (Simplex x) (Simplex y) -> Set.isSubset x y
    Array2D.init rows.Length cols.Length boundary |> Matrix

/// Computes the order (i.e. dimension) of a simplex.
let order (Simplex x) : int = Set.count x - 1

/// Dimension of a complex.
let dim (Complex c) : int =
    c
    |> Set.map order
    |> Seq.max

let size (Complex c) : Nat =
    c
    |> Set.count
    |> Nat

let count (sims : Set<Simplex>) : Nat =
    sims
    |> Set.count
    |> Nat

/// k-skeleton of a complex.
let skeleton (k : int) (Complex c) : Set<Simplex> = c |> Set.filter (fun s -> k = order s)

/// Skeletal decomposition.
let skelatalDecomposition (com : Complex) : Map<int, Set<Simplex>> = skelatalDecomposition dim skeleton com

/// Reduced boundary chain.
let reducedBoundaryChain (com : Complex) : Chain =
    if size com = Nat.Zero then Chain []
    else
        let l =
            List.map (snd >> Set.toList) (com
                                          |> skelatalDecomposition
                                          |> Map.toList
                                          |> List.sortBy fst)
        [ 0..l.Length - 2 ]
        |> List.map (fun i -> boundaryMatrix l.[i] l.[i + 1])
        |> List.toSeq
        |> Chain

/// Relative reduced boundary chain.
let relativeBoundaryChain (Complex c, Complex d) : Chain =
    c - d
    |> Complex
    |> reducedBoundaryChain

/// Boundary chain.
let boundaryChain (com : Complex) : Chain =
    let (Complex c) = com
    c
    |> Set.remove Empty
    |> Complex
    |> reducedBoundaryChain

// TODO
let star (com : Complex) = raise (System.NotImplementedException())
let flag (com : Complex) = raise (System.NotImplementedException())
let cone (com : Complex) = raise (System.NotImplementedException())
let alexandrov (com : Complex) = raise (System.NotImplementedException())
let compactHomology (com : Complex) = raise (System.NotImplementedException())
let localHomology (com : Complex) (star) = raise (System.NotImplementedException())
