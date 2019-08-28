/// This module defines all custom types.
[<AutoOpen>]
module Types

/// Label type of nodes.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Label =
    | Label of int
    member __.PrettyPrinter : string =
        let (Label s) = __
        string s

/// Matrix type over the binary field F2 (represented by `bool` with addition as `<>` and multiplication as `&&`). Represents vectors, matrices, vector spaces.
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
    member __.Item(r : int, c : int) : bool =
        let (Matrix m) = __
        m.[int r, int c]

    /// Gets row slice as a row vector.
    member __.GetSlice(r : int, c0Opt : int option, cnOpt : int option) : Matrix =
        let (Matrix m) = __
        let r' = int r
        let c0 = defaultArg c0Opt 0
        let cn = defaultArg cnOpt (Array2D.length2 m - 1)
        let col = m.[r', c0..cn]
        Array2D.init 1 (Array.length col) (fun _ c -> col.[c]) |> Matrix

    /// Gets column slice as a column vector.
    member __.GetSlice(r0Opt : int option, rnOpt : int option, c : int) : Matrix =
        let (Matrix m) = __
        let c' = int c
        let r0 = defaultArg r0Opt 0
        let rn = defaultArg rnOpt (Array2D.length1 m - 1)
        let row = m.[r0..rn, c']
        Array2D.init (Array.length row) 1 (fun r _ -> row.[r]) |> Matrix

    /// Gets block slice as a matrix.
    member __.GetSlice(r0Opt : int option, rnOpt : int option, c0Opt : int option, cnOpt : int option) : Matrix =
        let (Matrix m) = __
        let r0 = defaultArg r0Opt 0
        let rn = defaultArg rnOpt (Array2D.length1 m - 1)
        let c0 = defaultArg c0Opt 0
        let cn = defaultArg cnOpt (Array2D.length2 m - 1)
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

/// Chain complex.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Chain =
    | Chain of Matrix seq

    // Pretty printer.
    member __.PrettyPrinter : string =
        let (Chain a) = __
        a
        |> Seq.map (sprintf "%A")
        |> List.ofSeq
        |> String.concat "\n\n"

    /// Gets item.
    member __.Item(i : int) : Matrix =
        let (Chain m) = __
        (List.ofSeq m).[int i]

    /// Mapper.
    static member inline map (f : Matrix -> ^a) (Chain h) : ^a seq = Seq.map f h

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

/// Type alias for cochain.
type Cochain = Chain

/// Morphism of chains. TODO
type ChainMorphism = ChainMorphism of (Chain -> Chain)

/// Simplex type.
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Simplex =
    | Simplex of Label Set

    member __.PrettyPrinter : string =
        let (Simplex s) = __
        s
        |> Set.toList
        |> string

    /// Mapper.
    static member inline map (f : Label -> ^a) (Simplex x) : ^a Set = Set.map f x

    // Proper inclusion.
    static member (<.) (Simplex x, Simplex y) : bool = Set.isProperSubset x y
    // Inclusion.
    static member (<=.) (Simplex x, Simplex y) : bool = Set.isSubset x y

/// Simplicial complex type. Must satisfy complex relation (i.e. equal its powerset).
[<StructuredFormatDisplayAttribute("{PrettyPrinter}")>]
type Complex =
    | Complex of Simplex Set

    member __.PrettyPrinter : string =
        let (Complex c) = __
        c
        |> Set.toList
        |> List.map string
        |> String.concat " ; "

    /// Mapper.
    static member inline map (f : Simplex -> ^a) (Complex c) : ^a Set = Set.map f c

    // Proper inclusion.
    static member (<.) (Complex c, Complex d) : bool = Set.isProperSubset c d
    // Inclusion.
    static member (<=.) (Complex c, Complex d) : bool = Set.isSubset c d

/// Morphism of complexes.
type ComplexMorphism = (Simplex -> Simplex) * Complex * Complex

/// Sheaf.
type Sheaf =
    | Sheaf of Complex * ((Simplex * Simplex), Matrix)Map
    // Direct sum.
    static member (+) (a : Sheaf, b : Sheaf) : Sheaf = raise (System.NotImplementedException())
    // Tensor product.
    static member (*) (a : Sheaf, b : Sheaf) : Sheaf = raise (System.NotImplementedException())
    // Pullback.
    static member (<!) (f, a : Sheaf) : Sheaf = raise (System.NotImplementedException())
    // Pushforward.
    static member (!>) (f, a : Sheaf) : Sheaf = raise (System.NotImplementedException())

/// Type alias for cosheaf.
type Cosheaf = Sheaf

/// Morphism of sheaves. TODO
type SheafMophism = SheafMophism of (Sheaf -> Sheaf)
