/// This module contains functions related to the `Complex` type.
[<RequireQualifiedAccess>]
module Complex

/// The empty complex.
let Empty = set [ Simplex.Empty ] |> Complex

/// Computes the topological closure of a set of simplices.
let closure (sims : Simplex Set) : Complex =
    sims + set [ Simplex.Empty ]
    |> Set.map (fun (Simplex s) -> Helpers.powerset s)
    |> Set.unionMany
    |> Set.map Simplex
    |> Complex

/// Convenience constructor to produce a simplicial complex from a partial specification of the simplices (eg the facets).
let make (input : int list list) : Complex =
    input
    |> List.map (set
                 >> Set.map Label
                 >> Simplex)
    |> set
    |> closure

/// Returns the top-dimensional simplices of a complex.
let facets (Complex c) : Simplex Set =
    c - set [ for x in c do
                  for y in c do
                      if x <. y then yield x ]

/// Topological boundary.
let boundary (com : Complex) : Complex =
    let simplexBoundary (Simplex x) = x |> Set.map (fun v -> x - set [ v ] |> Simplex)
    com
    |> facets
    |> Set.map simplexBoundary
    |> Set.fold Helpers.symmetricDifference (set [ Simplex.Empty ])
    |> closure

/// The star of a simplex in a complex.
let star (Complex c, x : Simplex) : Complex =
    c
    |> Set.filter (fun y -> x <=. y)
    |> Complex

/// Dimension of a complex.
let dim (Complex c) : int =
    c
    |> Set.map Simplex.dim
    |> Seq.max

/// Size of a complex.
let size (Complex c) : int = c |> Set.count

/// k-skeleton of a complex.
let skeleton (k : int) (Complex c) : Simplex Set = c |> Set.filter (fun s -> k = Simplex.dim s)

/// Skelatal decomposition of a complex.
let skelatalDecomposition (com : Complex) : (int, Simplex Set)Map =
    [ -1..dim com ]
    |> List.map (fun k -> k, skeleton k com)
    |> Map.ofList

/// Skelatal decomposition of a complex as a list of lists.
let skelatalDecompositionList (com : Complex) : Simplex list list =
    List.map (snd >> Set.toList) (com
                                  |> skelatalDecomposition
                                  |> Map.toList
                                  |> List.sortBy fst)

/// Size of the k-skeleton of a complex.
let skeletonSize (k : int) (c : Complex) : int = skeleton k c |> Set.count

/// Reduced boundary chain.
let reducedBoundaryChain (com : Complex) : Chain =
    if size com = 0 then Chain []
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

/// Reduced coboundary chain.
let reducedCoboundaryCochain (com : Complex) : Cochain =
    com
    |> reducedBoundaryChain
    |> Chain.map Matrix.transpose
    |> Chain

/// Relative reduced coboundary chain.
let relativeCoboundaryChain (Complex c, Complex d) : Cochain =
    c - d
    |> Complex
    |> reducedCoboundaryCochain

/// Coboundary cochain.
let coboundaryCochain (com : Complex) : Cochain =
    let (Complex c) = com
    c
    |> Set.remove Simplex.Empty
    |> Complex
    |> reducedCoboundaryCochain

/// Betti numbers of a complex.
let betti (com : Complex) : int seq =
    com
    |> boundaryChain
    |> Chain.betti

/// k-th Betti number.
let kBetti (k : int) (com : Complex) : int =
    com
    |> boundaryChain
    |> Chain.kBetti k

/// Cobetti numbers of a complex.
let cobetti (com : Complex) : int seq =
    com
    |> coboundaryCochain
    |> Chain.cobetti

/// k-th Coetti number.
let kCobetti (k : int) (com : Complex) : int =
    com
    |> coboundaryCochain
    |> Chain.kBetti k

/// Homology of a complex.
let homology (com : Complex) : Chain =
    com
    |> boundaryChain
    |> Chain.homology

/// Relative homology of a complex
let relativeHomology (com : Complex, subcom : Complex) : Chain =
    (com, subcom)
    |> relativeBoundaryChain
    |> Chain.homology

/// Cohomology of a complex.
let cohomology (com : Complex) : Cochain =
    com
    |> coboundaryCochain
    |> Chain.cohomology

/// Relative cohomology of a complex
let relativeCohomology (com : Complex, subcom : Complex) : Cochain =
    (com, subcom)
    |> relativeCoboundaryChain
    |> Chain.cohomology

/// Local cohomology of a complex relative to a subsimplex.
let localCohomology (com : Complex, subsim : Simplex) : Cochain =
    let closedCell = set [ subsim ] |> closure
    let relativeCells = com - closedCell |> Complex
    (com, relativeCells)
    |> relativeCoboundaryChain
    |> Chain.cohomology

// TODO
let flag (com : Complex) = raise (System.NotImplementedException())
let cone (com : Complex) = raise (System.NotImplementedException())
let alexandrov (com : Complex) = raise (System.NotImplementedException())
let compactHomology (com : Complex) = raise (System.NotImplementedException())
let localHomology (com : Complex) (star) = raise (System.NotImplementedException())
