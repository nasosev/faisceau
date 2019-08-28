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
let length (Chain a) : int = Seq.length a

/// Reverse a chain.
let rev (Chain a) : Chain = Seq.rev a |> Chain

/// Dimensions of chain.
let dim (Chain a) : int seq = a |> Seq.map Matrix.dimCol

/// Row and columensions of chain.
let dims (Chain a) : (int * int) seq = a |> Seq.map Matrix.dim

/// Rank sequence of chain.
let rk (Chain a) : int seq = a |> Seq.map Matrix.rk

/// Euler characteristic.
let euler (Chain a) : int = a |> Seq.fold (fun acc m -> (Matrix.dimCol m) - acc) 0

/// Append zero to the end of a chain.
let augmentChain (Chain a) : Chain =
    Seq.append a [ Seq.last a
                   |> Matrix.dimCol
                   |> fun r -> Matrix.zero r 0 ]
    |> Chain

/// Append zero to the end of a cochain.
let augmentCochain (Chain a : Cochain) : Cochain =
    Seq.append a [ Seq.last a
                   |> Matrix.dimRow
                   |> fun c -> Matrix.zero 0 c ]
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
let private _chainBetti (Chain a) : int seq =
    let k = a |> Seq.map Matrix.nul

    let i =
        a
        |> Seq.map Matrix.rk
        |> Seq.tail
    Seq.map2 (-) k i

/// Betti numbers.
let betti (a : Cochain) : int seq =
    if length a = 0 then seq []
    else
        a
        |> augmentChain
        |> _chainBetti

/// k-th Betti number.
let kBetti (k : int) (a : Cochain) : int =
    match (Seq.tryItem k (betti a)) with
    | Some n -> n
    | None -> 0

/// Homology.
let homology (a : Cochain) : Cochain =
    if length a = 0 then Chain []
    else
        a
        |> augmentChain
        |> _chainHomology

/// Cobetti numbers.
let cobetti (a : Cochain) : int seq =
    if length a = 0 then seq []
    else
        a
        |> augmentCochain
        |> rev
        |> _chainBetti
        |> Seq.rev

/// k-th cobetti number.
let kCobetti (k : int) (a : Cochain) : int =
    match (Seq.tryItem k (cobetti a)) with
    | Some n -> n
    | None -> 0

/// Cohomology.
let cohomology (a : Cochain) : Cochain =
    if length a = 0 then Chain []
    else
        a
        |> augmentCochain
        |> rev
        |> _chainHomology
        |> rev
