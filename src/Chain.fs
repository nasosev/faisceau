/// This module contains functions related to the `Chain` type.
[<RequireQualifiedAccess>]
module Chain

// Convenience constructor with checking.
let chain (input : seq<Matrix>) : Chain =
    // Try to multiply consecutive elements to see if an exception occurs.
    let a = input |> Seq.toArray
    try
        for i in [ 0..-2 + Seq.length input ] do
            a.[i] * a.[i + 1] |> ignore
    with :? System.InvalidOperationException -> invalidOp "Cannot form a chain when boundary maps do not square to zero."
    // If no exception occurs, then give back a chain.
    Chain input

/// Dimensions of chain.
let dim (Chain h) : seq<Nat> = h |> Seq.map Matrix.dimCol

/// Row and columensions of chain.
let dims (Chain h) : seq<Nat * Nat> = h |> Seq.map Matrix.dim

/// Rank sequence of chain.
let rk (Chain h) : seq<Nat> = h |> Seq.map Matrix.rk

/// Euler characteristic.
let euler (Chain h) : int = h |> Seq.fold (fun acc m -> (Matrix.dimCol m |> int) - acc) 0

/// Betti numbers.
let betti (Chain h) : seq<Nat> =
    if Seq.isEmpty h then seq []
    else
        let nuls = h |> Seq.map Matrix.nul

        let rks =
            h
            |> Seq.map Matrix.rk
            |> Seq.tail
            |> fun x -> Seq.append x [ Nat.Zero ]
        Seq.map2 (-) nuls rks

/// Homology.
let homology (Chain h) : Chain =
    if Seq.isEmpty h then Chain []
    else
        let kers = h |> Seq.map Matrix.ker

        let ims =
            h
            |> Seq.map Matrix.im
            |> Seq.tail
            |> fun x ->
                Seq.append x [ Seq.last kers
                               |> Matrix.dim
                               ||> Matrix.zero ]
        Seq.map2 Matrix.quotient kers ims |> Chain
