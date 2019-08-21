/// This module contains random unit tests.
[<RequireQualifiedAccess>]
module RandomTests

open FsCheck

// To convert FsCheck's NonNegativeInto to our Nat.
let nnIntToNat = int >> Nat

type MatrixTests =
    static member ``transpose is idempotent`` (mat : Matrix) : bool = -(-mat) = mat

    static member ``sum is commutative`` (n' : NonNegativeInt) (m' : NonNegativeInt) : bool =
        let n, m = nnIntToNat n', nnIntToNat m'
        let mat1 = Matrix.random n m
        let mat2 = Matrix.random n m
        mat1 + mat2 = mat2 + mat1

    static member ``product is associative`` (n' : NonNegativeInt) (m' : NonNegativeInt) (k' : NonNegativeInt) (l' : NonNegativeInt) : bool =
        let n, m, k, l = nnIntToNat n', nnIntToNat m', nnIntToNat k', nnIntToNat l'
        let mat1 = Matrix.random n m
        let mat2 = Matrix.random m k
        let mat3 = Matrix.random k l
        (mat1 * mat2) * mat3 = mat1 * (mat2 * mat3)

    static member ``trace is cyclic`` (n' : NonNegativeInt) : bool =
        let n = nnIntToNat n'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random n n
        Matrix.tr (mat1 * mat2) = Matrix.tr (mat2 * mat1)

    static member ``trace commutes with tensor product`` (n' : NonNegativeInt) : bool =
        let n = nnIntToNat n'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random n n
        Matrix.tr (mat1 *! mat2) = (Matrix.tr (mat1) && Matrix.tr (mat2))

    static member ``rank-nullity`` (mat : Matrix) =
        let numberForm = Matrix.rk mat + Matrix.nul mat = Matrix.dimCol mat
        let spaceForm = Matrix.dimCol (Matrix.ker mat) + Matrix.dimCol (Matrix.im mat) = Matrix.dimCol mat
        numberForm && spaceForm

    static member ``determinant of product is product of determinants`` (n' : NonNegativeInt) : bool =
        let n = nnIntToNat n'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random n n
        Matrix.det (mat1 * mat2) = (Matrix.det mat1 && Matrix.det mat2)

    static member ``determinant of tensor product is product of determinants`` (n' : NonNegativeInt) (m' : NonNegativeInt) : bool =
        let n, m = nnIntToNat n', nnIntToNat m'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random m m
        Matrix.det (mat1 *! mat2) = (Matrix.det mat1 && Matrix.det mat2)

    static member ``product with inverse is identity`` (n' : NonNegativeInt) : Property =
        let n = int n' |> Nat
        let mat = Matrix.random n n
        Matrix.det mat ==> lazy
            let id = Matrix.identity n
            let inv = Matrix.inv mat
            (mat * inv = id) && (inv * mat = id)

type SimplexTests =

    static member ``closure is idempotent`` (xs : Set<Simplex>) : bool =
        let (Complex c) = Simplex.closure xs
        Complex c = Simplex.closure c

    static member ``closure of toplex is identity`` (xs : Set<Simplex>) : bool =
        let com = Simplex.closure xs
        com = (com
               |> Simplex.facets
               |> Simplex.closure)

    static member ``euler characteristic of chain and homology coincide`` (xs : Set<Simplex>) : bool =
        let com = xs |> Simplex.closure
        let chain = com |> Simplex.boundaryChain
        let homology = chain |> Chain.homology
        Chain.euler homology = Chain.euler chain

    static member ``betti is dimension of homology`` (xs : Set<Simplex>) : bool =
        let com = xs |> Simplex.closure

        let betti =
            com
            |> Simplex.boundaryChain
            |> Chain.betti
            |> List.ofSeq

        let homologyDims =
            com
            |> Simplex.boundaryChain
            |> Chain.homology
            |> Chain.dim
            |> List.ofSeq

        betti = homologyDims

    static member ``bettis exceeds reduced bettis by one in degree zero and coincide elsewhere`` (xs : Set<Simplex>) : Property =
        Simplex.count xs > Nat.One ==> lazy
            let com = xs |> Simplex.closure

            let betti =
                com
                |> Simplex.boundaryChain
                |> Chain.betti
                |> List.ofSeq

            let reducedBetti =
                com
                |> Simplex.reducedBoundaryChain
                |> Chain.betti
                |> List.ofSeq

            (List.head betti = Nat.One + List.head reducedBetti) && (List.tail betti = List.tail reducedBetti)

    static member ``boundary is nilpotent`` (xs : Set<Simplex>) =
        Simplex.EmptyComplex = (xs
                                |> Simplex.closure
                                |> Simplex.boundary
                                |> Simplex.boundary)

// type SheafTests =
//     static member ``cohomology of constant sheaf and complex coincide`` = ()
let testAll (endSize : int) : unit =
    let config = { Config.Default with EndSize = endSize }
    Check.All<MatrixTests>(config)
    Check.All<SimplexTests>(config)
// Check.All<SheafTests>(config)
