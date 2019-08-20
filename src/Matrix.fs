/// This module contains functions related to the `Matrix` type.
[<RequireQualifiedAccess>]
module Matrix

/// Convenience constructor to produce a matrix.
let matrix (input : List<List<int>>) : Matrix =
    if (Helpers.listNumDistinctListLengths input) <> 1 then invalidOp "Cannot make matrix from no row vectors or row vectors of unequal dimensions."
    let converter r c =
        match input.[r].[c] with
        | 1 -> true
        | _ -> false
    Array2D.init input.Length input.[0].Length converter |> Matrix

/// Row dimension.
let dimRow (Matrix m) : Nat = Array2D.length1 m |> Nat

/// Column dimension.
let dimCol (Matrix m) : Nat = Array2D.length2 m |> Nat

/// Matrix dimension.
let dim (mat : Matrix) : Nat * Nat = (dimRow mat, dimCol mat)

// Creates matrix with ones on diagonal of dimension (r,c).
let diagonal (Nat r) (Nat c) : Matrix = Array2D.init r c (=) |> Matrix
// Creates identity matrix of dimension n.
let identity (n : Nat) : Matrix = diagonal n n

/// Creates a zero matrix of dimension (r,c).
let zero (Nat r) (Nat c) : Matrix = Array2D.create r c false |> Matrix

// Creates a unit matrix of dimension (r,c).
let one (Nat r) (Nat c) : Matrix = Array2D.create r c true |> Matrix

// Creates a random matrix of dimension (r,c).
let random (Nat r) (Nat c) : Matrix =
    let rng = System.Random()
    Array2D.init r c (fun _ _ -> (rng.Next()) % 2 = 0) |> Matrix

/// Removes a row from a matrix.
let removeRow (mat : Matrix) (r : Nat) : Matrix = mat.[..r - Nat.One, *] +~ mat.[r + Nat.One.., *]

/// Removes a column from a matrix.
let removeCol (mat : Matrix) (c : Nat) : Matrix = mat.[*, ..c - Nat.One] +| mat.[*, c + Nat.One..]

/// Gives the Smith normal form of a matrix.
let smithNormalForm (Matrix m) : Matrix =
    let normal = Array2D.copy m
    let maxRowIndex = -1 + Array2D.length1 normal
    let maxColIndex = -1 + Array2D.length2 normal

    let exchangeRows i j =
        let x = normal.[i, *]
        normal.[i, *] <- normal.[j, *]
        normal.[j, *] <- x

    let exchangeCols i j =
        let x = normal.[*, i]
        normal.[*, i] <- normal.[*, j]
        normal.[*, j] <- x

    let addRow x i = normal.[i, *] <- Array.map2 (<>) normal.[x, *] normal.[i, *]
    let addCol x j = normal.[*, j] <- Array.map2 (<>) normal.[*, x] normal.[*, j]

    let rec reduce x =
        match Helpers.array2DtryFind true normal.[x.., x..] with
        | None -> ()
        | Some(i, j) ->
            exchangeRows x (i + x)
            exchangeCols x (j + x)
            for i in x + 1..maxRowIndex do
                if normal.[i, x] then addRow x i
                else ()
            for j in x + 1..maxColIndex do
                if normal.[x, j] then addCol x j
                else ()
            reduce (x + 1)
    reduce 0
    normal |> Matrix

/// Gives the Smith normal form factors `(left,right)` of the input matrix `input`, where `normal = left * input * right`.
let smithFormFactors (Matrix m) : Matrix * Matrix =
    let maxRowIndex = -1 + Array2D.length1 m
    let maxColIndex = -1 + Array2D.length2 m
    let rowDim = Array2D.length1 m
    let colDim = Array2D.length2 m
    let normal = Array2D.copy m
    let mutable left = identity (Nat rowDim)
    let mutable right = identity (Nat colDim)

    let exchangeRowsMatrix i j =
        let init r c =
            match r, c with
            | (x, y) when (x, y) = (i, j) -> true
            | (x, y) when (x, y) = (j, i) -> true
            | (x, y) when x = y && x <> i && x <> j -> true
            | _ -> false

        let matrix = Array2D.init rowDim rowDim init |> Matrix
        left <- matrix * left

    let addRowMatrix z i =
        let init r c =
            match r, c with
            | (x, y) when (x, y) = (i, z) -> true
            | (x, y) when x = y -> true
            | _ -> false

        let matrix = Array2D.init rowDim rowDim init |> Matrix
        left <- matrix * left

    let exchangeColsMatrix i j =
        let init r c =
            match r, c with
            | (x, y) when (x, y) = (i, j) -> true
            | (x, y) when (x, y) = (j, i) -> true
            | (x, y) when x = y && x <> i && x <> j -> true
            | _ -> false

        let matrix = Array2D.init colDim colDim init |> Matrix
        right <- right * matrix

    let addColMatrix z j =
        let init r c =
            match r, c with
            | (x, y) when (x, y) = (z, j) -> true
            | (x, y) when x = y -> true
            | _ -> false

        let matrix = Array2D.init colDim colDim init |> Matrix
        right <- right * matrix

    let exchangeRows i j =
        let x = normal.[i, *]
        normal.[i, *] <- normal.[j, *]
        normal.[j, *] <- x

    let exchangeCols i j =
        let x = normal.[*, i]
        normal.[*, i] <- normal.[*, j]
        normal.[*, j] <- x

    let addRow x i = normal.[i, *] <- Array.map2 (<>) normal.[x, *] normal.[i, *]
    let addCol x j = normal.[*, j] <- Array.map2 (<>) normal.[*, x] normal.[*, j]

    let rec reduce x =
        match Helpers.array2DtryFind true normal.[x.., x..] with
        | None -> ()
        | Some(i, j) ->
            exchangeRows x (i + x)
            exchangeRowsMatrix x (i + x)
            exchangeCols x (j + x)
            exchangeColsMatrix x (j + x)
            for i in x + 1..maxRowIndex do
                if normal.[i, x] then
                    addRow x i
                    addRowMatrix x i
                else ()
            for j in x + 1..maxColIndex do
                if normal.[x, j] then
                    addCol x j
                    addColMatrix x j
                else ()
            reduce (x + 1)
    reduce 0
    left, right

/// Gives the number of elementary divisors of a matrix.
let numElementaryDivisors (mat : Matrix) : Nat =
    let normal = mat |> smithNormalForm
    let diagLength = min (dimRow normal) (dimCol normal)
    if diagLength = Nat.Zero then Nat.Zero
    else
        [ Nat.Zero..diagLength - Nat.One ]
        |> List.map (fun i -> normal.[i, i])
        |> List.filter ((=) true)
        |> List.length
        |> Nat

/// Trace.
let tr (mat : Matrix) : bool =
    if dimRow mat <> dimCol mat then invalidOp "Cannot take trace of nonsquare matrix."
    let n = dimRow mat
    if n = Nat.Zero then false
    else
        [ Nat.Zero..n - Nat.One ]
        |> List.map (fun i -> mat.[i, i])
        |> List.reduce (<>)

/// Rank.
let rk (mat : Matrix) : Nat = numElementaryDivisors mat

/// Nullity.
let nul (mat : Matrix) : Nat = dimCol mat - rk mat

/// Inverse. Raises exception if matrix nonsquare or singular. TODO: alternatively use Moore-Penrose inverse?
let inv (mat : Matrix) : Matrix =
    if nul mat <> Nat.Zero || dimRow mat <> dimCol mat then invalidOp "Cannot invert singular matrix."
    let left, right = mat |> smithFormFactors
    right * left

/// Determinant.
let det (mat : Matrix) : bool =
    if dimRow mat <> dimCol mat then invalidOp "Cannot take determinant of nonsquare matrix."
    let normal = mat |> smithNormalForm
    let n = dimRow normal
    if n = Nat.Zero then false
    else
        [ Nat.Zero..n - Nat.One ]
        |> List.map (fun i -> normal.[i, i])
        |> List.reduce (&&)

/// Kernel.
let ker (mat : Matrix) : Matrix =
    let _, right = smithFormFactors mat
    let colIndex = dimCol mat - nul mat
    right.[*, colIndex..]

/// Image.
let im (mat : Matrix) : Matrix =
    let left, _ = smithFormFactors mat
    let i = numElementaryDivisors mat
    if i = Nat.Zero then zero (dimRow mat) Nat.Zero
    else
        let invLeft = inv left
        invLeft.[*, ..i - Nat.One]

/// Determines whether a vector is in the span of a matrix.
let inSpan (mat : Matrix) (vec : Matrix) : bool = rk mat = rk (mat +| vec)

/// Quotient vector space.
let quotient (space : Matrix) (subspace : Matrix) : Matrix =
    let d = dimCol subspace
    if d = Nat.Zero then space
    else
        let mutable extendedSubspace = subspace
        for i in Nat.Zero..(dimCol space) - Nat.One do
            if not (inSpan extendedSubspace space.[*, i]) then extendedSubspace <- extendedSubspace +| space.[*, i]
        extendedSubspace.[*, d..]

/// Pretty printer for common external formats.
let prettyPrint (brackets : string * string) (mat : Matrix) : string =
    let leftBracket, rightBracket = brackets

    let boolToStr =
        function
        | true -> "1"
        | false -> "0"

    let n, m = dimRow mat, dimCol mat
    let mutable s = "\n" + leftBracket + "\n"
    for i in Nat.Zero..n - Nat.One do
        for j in Nat.Zero..m - Nat.One do
            s <- s + match (i, j) with
                     | (x, y) when x = n - Nat.One && y = m - Nat.One -> boolToStr mat.[i, j] + rightBracket + "\n"
                     | (_, x) when x = Nat.Zero -> leftBracket + boolToStr mat.[i, j] + ", "
                     | (_, x) when x = m - Nat.One -> boolToStr mat.[i, j] + rightBracket + ",\n"
                     | _ -> boolToStr mat.[i, j] + ", "
    s + rightBracket + "\n"

/// Pretty print to Mathematica format.
let printMma (mat : Matrix) : string = prettyPrint ("{", "}") mat

/// Pretty print to SageMath format.
let printSage (mat : Matrix) : string = prettyPrint ("[", "]") mat
