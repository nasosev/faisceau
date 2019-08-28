/// This module contains functions related to the `Matrix` type.
[<RequireQualifiedAccess>]
module Matrix

/// Convenience constructor to produce a matrix.
let make (input : int list list) : Matrix =
    if (Helpers.listNumDistinctListLengths input) <> 1 then invalidOp "Cannot make matrix from no row vectors or row vectors of unequal dimensions."
    let converter r c =
        match input.[r].[c] with
        | 1 -> true
        | _ -> false
    Array2D.init input.Length input.[0].Length converter |> Matrix

/// Transpose.
let transpose (mat : Matrix) : Matrix = -mat

/// Row dimension.
let dimRow (Matrix m) : int = Array2D.length1 m

/// Column dimension.
let dimCol (Matrix m) : int = Array2D.length2 m

/// Row list.
let rows (mat : Matrix) : List<Matrix> = [ 0..(dimRow mat) - 1 ] |> List.map (fun i -> mat.[i, *])

/// Column list.
let cols (mat : Matrix) : List<Matrix> = [ 0..(dimCol mat) - 1 ] |> List.map (fun i -> mat.[*, i])

/// Delete duplicate rows.
let distinctRows (mat : Matrix) : Matrix =
    mat
    |> rows
    |> List.distinct
    |> List.reduce (+~)

/// Delete duplicate columns.
let distinctCols (mat : Matrix) : Matrix =
    mat
    |> cols
    |> List.distinct
    |> List.reduce (+|)

/// Matrix dimension.
let dim (mat : Matrix) : int * int = (dimRow mat, dimCol mat)

// Creates matrix with ones on diagonal of dimension (r,c).
let diagonal (r : int) (c : int) : Matrix = Array2D.init r c (=) |> Matrix
// Creates identity matrix of dimension n.
let identity (n : int) : Matrix = diagonal n n

/// Creates a zero matrix of dimension (r,c).
let zero (r : int) (c : int) : Matrix = Array2D.create r c false |> Matrix

// Creates a unit matrix of dimension (r,c).
let one (r : int) (c : int) : Matrix = Array2D.create r c true |> Matrix

// Creates a random matrix of dimension (r,c).
let random (r : int) (c : int) : Matrix =
    let rng = System.Random()
    Array2D.init r c (fun _ _ -> (rng.Next()) % 2 = 0) |> Matrix

/// Removes a row from a matrix.
let removeRow (mat : Matrix) (r : int) : Matrix = mat.[..r - 1, *] +~ mat.[r + 1.., *]

/// Removes a column from a matrix.
let removeCol (mat : Matrix) (c : int) : Matrix = mat.[*, ..c - 1] +| mat.[*, c + 1..]

/// Gives the Smith normal form of a matrix.
let rowReduce (dimCol : int) (Matrix m) : Matrix =
    let array = Array2D.copy m
    let maxRow = -1 + Array2D.length1 array
    let maxCol = -1 + (int dimCol)

    let swapRows i j =
        let x = array.[i, *]
        array.[i, *] <- array.[j, *]
        array.[j, *] <- x

    let swapCols i j =
        let x = array.[*, i]
        array.[*, i] <- array.[*, j]
        array.[*, j] <- x

    let addRow x i = array.[i, *] <- Array.map2 (<>) array.[x, *] array.[i, *]
    let addCol x j = array.[*, j] <- Array.map2 (<>) array.[*, x] array.[*, j]

    let rec reduce x =
        match Helpers.array2DtryFind true array.[x..maxRow, x..maxCol] with
        | None -> ()
        | Some(i, j) ->
            swapRows x (i + x)
            swapCols x (j + x)
            for i in x + 1..maxRow do
                if array.[i, x] then addRow x i
                else ()
            for j in x + 1..maxCol do
                if array.[x, j] then addCol x j
                else ()
            reduce (x + 1)
    reduce 0
    array |> Matrix

/// Gives the Smith normal form of a matrix.
let smithNormalForm (mat : Matrix) : Matrix = rowReduce (dimCol mat) mat

/// Solves the linear system A*X = B.
let linSolve (matA : Matrix) (matB : Matrix) : Matrix =
    let dimColA = dimCol matA
    let rowReduce = rowReduce dimColA (matA +| matB)
    rowReduce.[..dimColA - 1, dimColA..]

/// Gives the Smith normal form factors `(left,right)` of the input matrix `input`, where `normal = left * input * right`.
let smithFormFactors (Matrix m) : Matrix * Matrix =
    let rowDim = Array2D.length1 m
    let colDim = Array2D.length2 m
    let maxRow = -1 + rowDim
    let maxCol = -1 + colDim
    let array = Array2D.copy m
    let mutable left = identity rowDim
    let mutable right = identity colDim

    let swapRowsMatrix i j =
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

    let swapColsMatrix i j =
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

    let swapRows i j =
        let x = array.[i, *]
        array.[i, *] <- array.[j, *]
        array.[j, *] <- x

    let swapCols i j =
        let x = array.[*, i]
        array.[*, i] <- array.[*, j]
        array.[*, j] <- x

    let addRow x i = array.[i, *] <- Array.map2 (<>) array.[x, *] array.[i, *]
    let addCol x j = array.[*, j] <- Array.map2 (<>) array.[*, x] array.[*, j]

    let rec reduce x =
        match Helpers.array2DtryFind true array.[x.., x..] with
        | None -> ()
        | Some(i, j) ->
            swapRows x (i + x)
            swapRowsMatrix x (i + x)
            swapCols x (j + x)
            swapColsMatrix x (j + x)
            for i in x + 1..maxRow do
                if array.[i, x] then
                    addRow x i
                    addRowMatrix x i
                else ()
            for j in x + 1..maxCol do
                if array.[x, j] then
                    addCol x j
                    addColMatrix x j
                else ()
            reduce (x + 1)
    reduce 0
    left, right

/// Gives the number of elementary divisors of a matrix.
let numElementaryDivisors (mat : Matrix) : int =
    let normal = mat |> smithNormalForm
    let diagLength = min (dimRow normal) (dimCol normal)
    if diagLength = 0 then 0
    else
        [ 0..diagLength - 1 ]
        |> List.map (fun i -> normal.[i, i])
        |> List.filter ((=) true)
        |> List.length

/// Trace.
let tr (mat : Matrix) : bool =
    if dimRow mat <> dimCol mat then invalidOp "Cannot take trace of nonsquare matrix."
    let n = dimRow mat
    if n = 0 then false
    else
        [ 0..n - 1 ]
        |> List.map (fun i -> mat.[i, i])
        |> List.reduce (<>)

/// Rank.
let rk (mat : Matrix) : int = numElementaryDivisors mat

/// Nullity.
let nul (mat : Matrix) : int = dimCol mat - rk mat

/// Inverse.
let inv (mat : Matrix) : Matrix =
    if nul mat <> 0 || dimRow mat <> dimCol mat then invalidOp "Cannot invert singular matrix."
    let left, right = mat |> smithFormFactors
    right * left

/// Determinant.
let det (mat : Matrix) : bool =
    if dimRow mat <> dimCol mat then invalidOp "Cannot take determinant of nonsquare matrix."
    let normal = mat |> smithNormalForm
    let n = dimRow normal
    if n = 0 then false
    else
        [ 0..n - 1 ]
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
    if i = 0 then zero (dimRow mat) 0
    else
        let invLeft = inv left
        invLeft.[*, ..i - 1]

/// Determines whether a vector is in the span of a matrix.
let inSpan (mat : Matrix) (vec : Matrix) : bool = rk mat = rk (mat +| vec)

/// Quotient vector space.
let quotient (space : Matrix) (subspace : Matrix) : Matrix =
    let d = dimCol subspace
    if d = 0 then space
    else
        let mutable extendedSubspace = subspace
        for i in 0..(dimCol space) - 1 do
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
    for i in 0..n - 1 do
        for j in 0..m - 1 do
            s <- s + match (i, j) with
                     | (x, y) when x = n - 1 && y = m - 1 -> boolToStr mat.[i, j] + rightBracket + "\n"
                     | (_, x) when x = 0 -> leftBracket + boolToStr mat.[i, j] + ", "
                     | (_, x) when x = m - 1 -> boolToStr mat.[i, j] + rightBracket + ",\n"
                     | _ -> boolToStr mat.[i, j] + ", "
    s + rightBracket + "\n"

/// Pretty print to Mathematica format.
let printMma (mat : Matrix) : string = prettyPrint ("{", "}") mat

/// Pretty print to SageMath format.
let printSage (mat : Matrix) : string = prettyPrint ("[", "]") mat
