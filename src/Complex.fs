/// This module contains functions related to the `Complex` type.
[<RequireQualifiedAccess>]
module Complex

/// The empty complex.
let Empty = set [ Simplex.Empty ] |> Complex

/// Computes the topological closure of a set of simplices.
let closure (sims : Set<Simplex>) : Complex =
    sims + set [ Simplex.Empty ]
    |> Set.map (fun (Simplex s) -> Helpers.powerset s)
    |> Set.unionMany
    |> Set.map Simplex
    |> Complex

/// Convenience constructor to produce a simplicial complex from a partial specification of the simplices (eg the facets).
let make (input : int list list) : Complex =
    List.map (set >> ((fun x -> x |> Set.map Label) >> Simplex)) input
    |> set
    |> closure

/// Returns the top-dimensional simplices of a complex.
let facets (Complex c) : Set<Simplex> =
    c - set [ for x in c do
                  for y in c do
                      if x <. y then yield x ]

/// Topological boundary.
let boundary (com : Complex) =
    let simplexBoundary (Simplex x) = x |> Set.map (fun v -> x - set [ v ] |> Simplex)
    com
    |> facets
    |> Set.map simplexBoundary
    |> Set.fold Helpers.symmetricDifference (set [ Simplex.Empty ])
    |> closure

/// The star of a simplex in a complex.
let star (Complex c, x : Simplex) =
    c
    |> Set.filter (fun y -> x <=. y)
    |> Complex

/// Dimension of a complex.
let dim (Complex c) : int =
    c
    |> Set.map Simplex.dim
    |> Seq.max

let size (Complex c) : Nat =
    c
    |> Set.count
    |> Nat

/// k-skeleton of a complex.
let skeleton (k : int) (Complex c) : Set<Simplex> = c |> Set.filter (fun s -> k = Simplex.dim s)

/// Generic function to compute a skelatal decomposition.
let skelatalDecomposition (com : Complex) : Map<int, Set<Simplex>> =
    [ -1..dim com ]
    |> List.map (fun k -> k, skeleton k com)
    |> Map.ofList

let skelatalDecompositionList (com : Complex) : Simplex list list =
    List.map (snd >> Set.toList) (com
                                  |> skelatalDecomposition
                                  |> Map.toList
                                  |> List.sortBy fst)

/// Size of the k-skeleton of a complex.
let skeletonSize (k : int) (c : Complex) : Nat =
    skeleton k c
    |> Set.count
    |> Nat

/// Reduced boundary chain.
let reducedBoundaryChain (com : Complex) : Chain =
    if size com = Nat.Zero then Chain []
    else
        let skeleton = com |> skelatalDecompositionList
        [ 0..skeleton.Length - 2 ]
        |> List.map (fun i -> Simplex.boundaryMatrix skeleton.[i] skeleton.[i + 1])
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
    |> Set.remove Simplex.Empty
    |> Complex
    |> reducedBoundaryChain

/// Betti numbers of a complex.
let betti (com : Complex) : Nat seq =
    com
    |> boundaryChain
    |> Chain.betti

/// Homology of a complex.
let homology (com : Complex) : Chain =
    com
    |> boundaryChain
    |> Chain.homology

// TODO
let flag (com : Complex) = raise (System.NotImplementedException())
let cone (com : Complex) = raise (System.NotImplementedException())
let alexandrov (com : Complex) = raise (System.NotImplementedException())
let compactHomology (com : Complex) = raise (System.NotImplementedException())
let localHomology (com : Complex) (star) = raise (System.NotImplementedException())
