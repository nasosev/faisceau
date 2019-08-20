/// This module contains generic functions.
module Generics

/// Generic function to compute a skelatal decomposition.
let skelatalDecomposition (dim : 'a -> int) (skeleton : int -> 'a -> 'a0) (object : 'a) : Map<int, 'a0> =
    [ -1..dim object ]
    |> List.map (fun k -> k, skeleton k object)
    |> Map.ofList
