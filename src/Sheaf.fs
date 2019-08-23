/// This module contains functions related to the `Sheaf` and `Cosheaf` types.
[<RequireQualifiedAccess>]
module Sheaf

open Generics

/// Convenience constructor to produce a sheaf.
let make (input : List<List<int> * List<int> * Matrix>) : Sheaf =
    let intListtoSimplex x =
        x
        |> List.map Label
        |> Set.ofList
        |> Simplex
    input
    |> List.map (fun (k1, k2, value) -> (intListtoSimplex k1, intListtoSimplex k2), value)
    |> Map.ofList
    |> Sheaf

/// Dimension.
let dim (Sheaf f) : int =
    f
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.collect (fun (x, y) -> [ x; y ])
    |> Seq.map Simplex.dim
    |> Seq.max

/// k-Skeleton.
let skeleton (k : int) (Sheaf f) : Sheaf =
    f
    |> Map.filter (fun (_, x) _ -> Simplex.dim x = k)
    |> Sheaf

/// Skeletal decomposition.
let skelatalDecomposition (sheaf : Sheaf) : Map<int, Sheaf> = skelatalDecomposition dim skeleton sheaf

// Creates a block matrix from the relative boundary data of two sets of simplices.
let private _blockCompose (rows : Set<Simplex>) (cols : Set<Simplex>) (Sheaf f) : Matrix =
    let rowArray =
        rows
        |> Set.toArray
        |> Array.sort

    let colArray =
        cols
        |> Set.toArray
        |> Array.sort

    let selector r c = f |> Map.tryFind (rowArray.[r], colArray.[c])
    let blockArray = Array2D.init (Array.length rowArray) (Array.length colArray) selector

    let dimArray =
        blockArray
        |> Array2D.map (fun x ->
               match x with
               | None -> 0, 0
               | Some x -> Matrix.dimRow x |> int, Matrix.dimCol x |> int)

    let rowDim row =
        row
        |> Array.map (fst >> Nat)
        |> Array.reduce max

    let colDim col =
        col
        |> Array.map (snd >> Nat)
        |> Array.reduce max

    let rowDims = [| 0..-1 + Array.length rowArray |] |> Array.map (fun r -> rowDim dimArray.[r, *])
    let colDims = [| 0..-1 + Array.length colArray |] |> Array.map (fun c -> colDim dimArray.[*, c])

    let fullBlockArray =
        blockArray
        |> Array2D.mapi (fun r c x ->
               match x with
               | Some x -> x
               | None -> Matrix.zero rowDims.[r] colDims.[c])
    [| 0..Array.length rowArray - 1 |]
    |> Array.map (fun i -> Array.reduce (+|) fullBlockArray.[i, *])
    |> Array.reduce (+~)

/// Creates the k-th boundary matrix of a sheaf.
let boundaryMatrix (k : int) (sheaf : Sheaf) : Matrix =
    let kSkeleton = skeleton k sheaf
    let (Sheaf f) = sheaf

    let rows =
        f
        |> Map.toSeq
        |> Seq.map (fun ((x, _), _) -> x)
        |> Set.ofSeq

    let cols =
        f
        |> Map.toSeq
        |> Seq.map (fun ((_, y), _) -> y)
        |> Set.ofSeq

    _blockCompose rows cols kSkeleton

/// Creates the boundary chain of a sheaf.
let boundaryChain (sheaf : Sheaf) : Chain =
    [ 0..dim sheaf ]
    |> List.map (fun k -> boundaryMatrix k sheaf)
    |> List.rev
    |> List.toSeq
    |> Chain

/// Outputs the unreduced cobetti numbers of a sheaf.
let unreducedCobetti (sheaf : Sheaf) : List<Nat> =
    sheaf
    |> boundaryChain
    |> Chain.betti
    |> Seq.toList
    |> List.rev

/// Outputs the cohomology of a sheaf.
let cohomology (sheaf : Sheaf) : Chain =
    let homology =
        sheaf
        |> boundaryChain
        |> Chain.homology

    let (Chain a) = homology
    a
    |> Seq.rev
    |> Chain
