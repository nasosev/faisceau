/// This module contains functions related to the `Sheaf` and `Cosheaf` types.
[<RequireQualifiedAccess>]
module Sheaf

/// Convenience constructor to produce a sheaf.
let make (input : Complex * (int list * int list * Matrix) list) : Sheaf =
    let com = fst input

    let intListtoSimplex x =
        x
        |> List.map Label
        |> Set.ofList
        |> Simplex

    let maps =
        input
        |> snd
        |> List.map (fun (k1, k2, value) -> (intListtoSimplex k1, intListtoSimplex k2), value)
        |> Map.ofList

    (com, maps) |> Sheaf

/// Dimension.
let dim (Sheaf(com, _)) : int = Complex.dim com

/// Size.
let size (Sheaf(com, maps)) : int * int = Complex.size com, Map.count maps

/// Coboundary matrix of a sheaf.
let coboundaryMatrix (rows : Simplex list) (cols : Simplex list) (maps : ((Simplex * Simplex), Matrix)Map) : Matrix =
    match (List.length rows, List.length cols) with
    | 0, 0 -> Matrix.zero 0 0
    | _, 0 ->
        rows
        |> List.fold (fun acc sim -> acc + (Matrix.dimRow maps.[sim, sim])) 0
        |> fun n -> Matrix.zero n 0
    | 0, _ ->
        cols
        |> List.fold (fun acc sim -> acc + (Matrix.dimCol maps.[sim, sim])) 0
        |> Matrix.zero 0
    | _ ->
        let rowArray = rows |> List.toArray
        let colArray = cols |> List.toArray

        let selector r c =
            maps
            |> Map.tryFind (rowArray.[r], colArray.[c])
            |> function
            | Some mat -> mat
            | None -> Matrix.zero (Matrix.dimRow maps.[rowArray.[r], rowArray.[r]]) (Matrix.dimCol maps.[colArray.[c], colArray.[c]])

        let blockArray = Array2D.init (Array.length rowArray) (Array.length colArray) selector
        [| 0..Array2D.length1 blockArray - 1 |]
        |> Array.map (fun i -> Array.reduce (+|) blockArray.[i, *])
        |> Array.reduce (+~)

/// Reduced coboundary chain of a sheaf.
let reducedCoboundaryCochain (Sheaf(com, maps)) : Cochain =
    if Complex.size com = 0 then Chain []
    else
        let skeleton = com |> Complex.skelatalDecompositionList
        [ 0..skeleton.Length - 2 ]
        |> List.map (fun i -> coboundaryMatrix skeleton.[i + 1] skeleton.[i] maps)
        |> List.toSeq
        |> Chain

/// Boundary chain.
let coboundaryCochain (Sheaf(com, maps)) : Cochain =
    let (Complex c) = com
    c
    |> Set.remove Simplex.Empty
    |> Complex
    |> fun reducedCom -> reducedCoboundaryCochain (Sheaf(reducedCom, maps))

/// Cobetti numbers of a sheaf.
let cobetti (sheaf : Sheaf) : int seq =
    sheaf
    |> coboundaryCochain
    |> Chain.cobetti

/// Cohomology of a sheaf.
let cohomology (sheaf : Sheaf) : Cochain =
    sheaf
    |> coboundaryCochain
    |> Chain.cohomology

/// Zero sheaf on a complex.
let zero (com : Complex) : Sheaf = raise (System.NotImplementedException())

/// Skyscraper sheaf over a simplex of a complex.
let skyscraper (com : Complex, sim : Simplex) : Sheaf = raise (System.NotImplementedException())

/// Constant sheaf on a complex.
let constant (com : Complex) : Sheaf =
    let (Complex c) = com

    let selfMaps =
        c
        |> Set.map (fun x -> (x, x), Matrix.identity 1)
        |> Set.toList

    let otherMaps =
        seq {
            for x in c do
                let d = 1 + Simplex.dim x
                let higher = c |> Set.filter (fun y -> Simplex.dim y = d)
                for y in higher do
                    if x <=. y then yield (y, x), Matrix.identity 1
        }
        |> Seq.toList

    let allMaps = selfMaps @ otherMaps |> Map.ofList
    (com, allMaps) |> Sheaf

/// Orientation sheaf on a complex.
let orientation (com : Complex) : Sheaf = raise (System.NotImplementedException())
