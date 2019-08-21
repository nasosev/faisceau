﻿/// This module defines all custom types.
[<AutoOpen>]
module Types

/// Nonnegative integer type.
type Nat =
    | Nat of int
    static member Zero = Nat 0
    static member One = Nat 1
    static member (+) (Nat n, Nat m) : Nat = abs n + abs m |> Nat
    static member (-) (Nat n, Nat m) : Nat = max 0 (abs n - abs m) |> Nat
    static member (*) (Nat n, Nat m) : Nat = abs n * abs m |> Nat
    static member op_Explicit (Nat n) : int = n

/// Label type of nodes.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Label =
    | Label of int
    member __.PrettyPrinter : string =
        let (Label s) = __
        string s

/// Matrix type over the binary field F2 (represented by `bool` with addition as `<>` and multiplication as `&&`). Represents vectors, matrices, vector spaces. Module: `Matrix`.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Matrix =
    | Matrix of bool [,]

    member __.PrettyPrinter : string =
        let (Matrix m) = __

        let converter =
            function
            | true -> 1
            | false -> 0
        m
        |> Array2D.map converter
        |> sprintf "%A"

    /// Gets item as a bool.
    member __.Item(r : Nat, c : Nat) : bool =
        let (Matrix m) = __
        m.[int r, int c]

    /// Gets row slice as a row vector.
    member __.GetSlice(r : Nat, c0Opt : Nat option, cnOpt : Nat option) : Matrix =
        let (Matrix m) = __
        let r' = int r
        let c0 = defaultArg c0Opt Nat.Zero |> int
        let cn = defaultArg cnOpt (Nat(Array2D.length2 m - 1)) |> int
        let col = m.[r', c0..cn]
        Array2D.init 1 (Array.length col) (fun _ c -> col.[c]) |> Matrix

    /// Gets column slice as a column vector.
    member __.GetSlice(r0Opt : Nat option, rnOpt : Nat option, c : Nat) : Matrix =
        let (Matrix m) = __
        let c' = int c
        let r0 = defaultArg r0Opt Nat.Zero |> int
        let rn = defaultArg rnOpt (Nat(Array2D.length1 m - 1)) |> int
        let row = m.[r0..rn, c']
        Array2D.init (Array.length row) 1 (fun r _ -> row.[r]) |> Matrix

    /// Gets block slice as a matrix.
    member __.GetSlice(r0Opt : Nat option, rnOpt : Nat option, c0Opt : Nat option, cnOpt : Nat option) : Matrix =
        let (Matrix m) = __
        let r0 = defaultArg r0Opt Nat.Zero |> int
        let rn = defaultArg rnOpt (Nat(Array2D.length1 m - 1)) |> int
        let c0 = defaultArg c0Opt Nat.Zero |> int
        let cn = defaultArg cnOpt (Nat(Array2D.length2 m - 1)) |> int
        m.[r0..rn, c0..cn] |> Matrix

    /// Transpose.
    static member (~-) (Matrix m) : Matrix =
        let transposer r c = m.[c, r]
        transposer
        |> Array2D.init (Array2D.length2 m) (Array2D.length1 m)
        |> Matrix

    /// Sum.
    static member (+) (Matrix m1, Matrix m2) : Matrix =
        let r1, c1, r2, c2 = Array2D.length1 m1, Array2D.length2 m1, Array2D.length1 m2, Array2D.length2 m2
        if (r1 <> r2) || (c1 <> c2) then invalidOp "Cannot sum matrices of unequal dimensions."
        let adder r c = m1.[r, c] <> m2.[r, c]
        adder
        |> Array2D.init r1 c1
        |> Matrix

    /// Product.
    static member (*) (Matrix m1, Matrix m2) : Matrix =
        let r1, c1, r2, c2 = Array2D.length1 m1, Array2D.length2 m1, Array2D.length1 m2, Array2D.length2 m2
        if c1 <> r2 then invalidOp "Cannot multiply matrices of incompatible dimensions."
        let multiplier r c = (m1.[r, *], m2.[*, c]) ||> Array.fold2 (fun a x1 x2 -> a <> (x1 && x2)) false
        multiplier
        |> Array2D.init r1 c2
        |> Matrix

    /// Direct sum.
    static member (+!) (Matrix m1, Matrix m2) : Matrix =
        let r1, c1, r2, c2 = Array2D.length1 m1, Array2D.length2 m1, Array2D.length1 m2, Array2D.length2 m2
        let result = Array2D.zeroCreate (r1 + r2) (c1 + c2)
        Array2D.blit m1 0 0 result 0 0 r1 c1
        Array2D.blit m2 0 0 result r1 c1 r2 c2
        result |> Matrix

    /// Row direct sum.
    static member (+~) (Matrix m1, Matrix m2) : Matrix =
        let r1, c1, r2, c2 = Array2D.length1 m1, Array2D.length2 m1, Array2D.length1 m2, Array2D.length2 m2
        if c1 <> c2 then invalidOp "Cannot row direct sum with matrices of unequal column dimensions."
        let result = Array2D.zeroCreate (r1 + r2) c1
        Array2D.blit m1 0 0 result 0 0 r1 c1
        Array2D.blit m2 0 0 result r1 0 r2 c2
        result |> Matrix

    /// Column direct sum.
    static member (+|) (Matrix m1, Matrix m2) : Matrix =
        let r1, c1, r2, c2 = Array2D.length1 m1, Array2D.length2 m1, Array2D.length1 m2, Array2D.length2 m2
        if r1 <> r2 then invalidOp "Cannot column direct sum with matrices of unequal row dimensions."
        let result = Array2D.zeroCreate r1 (c1 + c2)
        Array2D.blit m1 0 0 result 0 0 r1 c1
        Array2D.blit m2 0 0 result 0 c1 r2 c2
        result |> Matrix

    /// Tensor product.
    static member ( *! ) (Matrix m1, Matrix m2) : Matrix =
        let r1, c1, r2, c2 = Array2D.length1 m1, Array2D.length2 m1, Array2D.length1 m2, Array2D.length2 m2
        let rr = r1 * r2
        let cc = c1 * c2
        let result = Array2D.zeroCreate rr cc
        for i in 1..rr do
            for j in 1..cc do
                let k1 = (i - 1) / r2
                let l1 = (j - 1) / c2
                let k2 = (i - 1) % r2
                let l2 = (j - 1) % c2
                result.[i - 1, j - 1] <- m1.[k1, l1] && m2.[k2, l2]
        result |> Matrix

/// Chain complex. Module: `Chain`.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Chain =
    | Chain of seq<Matrix>

    // Pretty printer.
    member __.PrettyPrinter : string =
        let (Chain a) = __
        a
        |> Seq.map (sprintf "%A")
        |> List.ofSeq
        |> String.concat "\n\n"

    /// Gets item.
    member __.Item(i : Nat) : Matrix =
        let (Chain m) = __
        (List.ofSeq m).[int i]

    // Chain product, tensor product of chains. Can be used on the (unreduced) homology of simplicial complexes to obtain the homology of their cartesian product (Kunneth formula).
    static member ( *! ) (Chain a, Chain b) : Chain =
        let antiDiagonal n m k =
            { 0..k }
            |> fun x -> x, Seq.rev x
            ||> Seq.zip
            |> Seq.filter (fun (i, j) -> i < n && j < m)
            |> Seq.toArray

        let a', b' = a |> Seq.toArray, b |> Seq.toArray
        let an, bn = Array.length a', Array.length b'
        let emptyMatrix = Array2D.zeroCreate 0 0 |> Matrix
        Array.map ((antiDiagonal an bn) >> (Array.fold (fun acc (i, j) -> acc +! a'.[i] *! b'.[j]) emptyMatrix)) [| 0..max an bn |]
        |> Array.toSeq
        |> Chain

/// Simplex type. Module: `Simplex`.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Simplex =
    | Simplex of Set<Label>
    member __.PrettyPrinter : string =
        let (Simplex s) = __
        s
        |> Set.toList
        |> string

/// Simplicial complex type. Must satisfy complex relation (i.e. equal its powerset). Module: `Simplex`.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Complex =
    | Complex of Set<Simplex>
    member __.PrettyPrinter : string =
        let (Complex c) = __
        c
        |> Set.toList
        |> string

/// Sheaf. Module: `Sheaf`.
type Sheaf = Sheaf of Map<Simplex * Simplex, Matrix>

/// Cosheaf. Module: `Sheaf`.
type Cosheaf = Cosheaf of Sheaf
