/// This module contains functions related to the `Chain` and `Cochain` types.
[<RequireQualifiedAccess>]
module Chain

// Convenience constructor with checking.
let make (input : Matrix seq) : Chain =
    // Try to multiply consecutive elements to see if an exception occurs.
    let a = input |> Seq.toArray
    try
        for i in [ 0..-2 + Seq.length input ] do
            a.[i] * a.[i + 1] |> ignore
    with :? System.InvalidOperationException -> invalidOp "Cannot form a chain when boundary maps do not square to zero."
    // If no exception occurs, then give back a chain.
    Chain input

/// Length of a chain.
let length (Chain a) : Nat = Seq.length a |> Nat

/// Reverse a chain.
let rev (Chain a) : Chain = Seq.rev a |> Chain

/// Dimensions of chain.
let dim (Chain a) : Nat seq = a |> Seq.map Matrix.dimCol

/// Row and columensions of chain.
let dims (Chain a) : (Nat * Nat) seq = a |> Seq.map Matrix.dim

/// Rank sequence of chain.
let rk (Chain a) : Nat seq = a |> Seq.map Matrix.rk

/// Euler characteristic.
let euler (Chain a) : int = a |> Seq.fold (fun acc m -> (Matrix.dimCol m |> int) - acc) 0

/// Append zero to the end of a chain.
let private _augmentChain (Chain a) : Chain =
    Seq.append a [ Seq.last a
                   |> Matrix.dimCol
                   |> fun r -> Matrix.zero r Nat.Zero ]
    |> Chain

/// Append zero to the end of a cochain.
let private _augmentCochain (Chain a : Cochain) : Cochain =
    Seq.append a [ Seq.last a
                   |> Matrix.dimRow
                   |> fun c -> Matrix.zero Nat.Zero c ]
    |> Chain

/// Chain homology.
let private _chainHomology (Chain a) : Chain =
    let k = a |> Seq.map Matrix.ker

    let i =
        a
        |> Seq.map Matrix.im
        |> Seq.tail
    Seq.map2 Matrix.quotient k i |> Chain

/// Chain betti.
let private _chainBetti (Chain a) : Nat seq =
    let k = a |> Seq.map Matrix.nul

    let i =
        a
        |> Seq.map Matrix.rk
        |> Seq.tail
    Seq.map2 (-) k i

/// Betti numbers.
let betti (a : Cochain) : Nat seq =
    if length a = Nat.Zero then seq []
    else
        a
        |> _augmentChain
        |> _chainBetti

/// Homology.
let homology (a : Cochain) : Cochain =
    if length a = Nat.Zero then Chain []
    else
        a
        |> _augmentChain
        |> _chainHomology

/// Cobetti numbers.
let cobetti (a : Cochain) : Nat seq =
    if length a = Nat.Zero then seq []
    else
        a
        |> _augmentCochain
        |> rev
        |> _chainBetti
        |> Seq.rev

/// Cohomology.
let cohomology (a : Cochain) : Cochain =
    if length a = Nat.Zero then Chain []
    else
        a
        |> _augmentCochain
        |> rev
        |> _chainHomology
        |> rev
