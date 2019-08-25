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
let size (Sheaf(com, maps)) : Nat * Nat = Complex.size com, Map.count maps |> Nat

/// Creates the coboundary matrix of a sheaf.
let coboundaryMatrix (rows : Simplex list) (cols : Simplex list) (maps : (Simplex * Simplex, Matrix) Map) : Matrix =
    match (List.length rows, List.length cols) with
    | 0, 0 -> Matrix.zero Nat.Zero Nat.Zero
    | _, 0 ->
        rows
        |> List.fold (fun acc sim -> acc + (Matrix.dimRow maps.[sim, sim])) Nat.Zero
        |> fun n -> Matrix.zero n Nat.Zero
    | 0, _ ->
        cols
        |> List.fold (fun acc sim -> acc + (Matrix.dimCol maps.[sim, sim])) Nat.Zero
        |> Matrix.zero Nat.Zero
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

/// Makes the reduced coboundary chain of a sheaf.
let reducedCoboundaryCochain (Sheaf(com, maps)) : Chain =
    if Complex.size com = Nat.Zero then Chain []
    else
        let skeleton = com |> Complex.skelatalDecompositionList
        [ 0..skeleton.Length - 2 ]
        |> List.map (fun i -> coboundaryMatrix skeleton.[i + 1] skeleton.[i] maps)
        |> List.toSeq
        |> Chain

/// Boundary chain.
let coboundaryCochain (Sheaf(com, maps)) : Chain =
    let (Complex c) = com
    c
    |> Set.remove Simplex.Empty
    |> Complex
    |> fun reducedCom -> reducedCoboundaryCochain (Sheaf(reducedCom, maps))

/// Cobetti numbers of a sheaf.
let cobetti (sheaf : Sheaf) : Nat seq =
    sheaf
    |> coboundaryCochain
    |> Chain.cobetti

/// Cohomology of a sheaf.
let cohomology (sheaf : Sheaf) : Chain =
    sheaf
    |> coboundaryCochain
    |> Chain.cohomology

/// Makes the zero sheaf on a complex.
let zero (com : Complex) : Sheaf = raise (System.NotImplementedException())

/// Makes the skyscraper sheaf over a simplex of a complex.
let skyscraper (com : Complex, sim : Simplex) : Sheaf = raise (System.NotImplementedException())

/// Makes the constant sheaf on a complex.
let constant (com : Complex) : Sheaf =
    let (Complex c) = com

    let identityMaps =
        c
        |> Set.map (fun x -> (x, x), Matrix.identity Nat.One)
        |> Set.toList

    let maps =
        seq {
            for x in c do
                let d = 1 + Simplex.dim x
                let higher = c |> Set.filter (fun y -> Simplex.dim y = d)
                for y in higher do
                    if x <=. y then yield (y, x), Matrix.identity Nat.One
        }
        |> Seq.toList

    let allMaps = identityMaps @ maps |> Map.ofList
    (com, allMaps) |> Sheaf
