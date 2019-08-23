/// This module contains functions related to the `Simplex` type.
[<RequireQualifiedAccess>]
module Simplex

/// The empty simplex.
let Empty = set [] |> Simplex

/// Computes the dimension of a simplex.
let dim (Simplex x) : int = Set.count x - 1

/// Computes the boundary matrix associated to two lists of simplices.
let boundaryMatrix (rows : List<Simplex>) (cols : List<Simplex>) : Matrix =
    let boundary r c = (rows.[r], cols.[c]) ||> (<=.)
    Array2D.init rows.Length cols.Length boundary |> Matrix
