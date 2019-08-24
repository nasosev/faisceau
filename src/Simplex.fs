/// This module contains functions related to the `Simplex` type.
[<RequireQualifiedAccess>]
module Simplex

/// The empty simplex.
let Empty = set [] |> Simplex

/// Computes the dimension of a simplex.
let dim (Simplex x) : int = Set.count x - 1

/// Computes the boundary matrix associated to two lists of simplices.
let boundaryMatrix (rows : List<Simplex>) (cols : List<Simplex>) : Matrix =
    let rowArray = rows |> List.toArray
    let colArray = cols |> List.toArray
    let boundary r c = (rowArray.[r], colArray.[c]) ||> (<=.)
    Array2D.init rows.Length cols.Length boundary |> Matrix
