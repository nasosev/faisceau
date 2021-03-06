﻿/// This module contains random unit tests.
[<RequireQualifiedAccess>]
module RandomTests

open FsCheck

type MatrixTests =
    static member ``transpose is idempotent`` (mat : Matrix) : bool = -(-mat) = mat

    static member ``sum is commutative`` (n' : NonNegativeInt) (m' : NonNegativeInt) : bool =
        let n, m = int n', int m'
        let mat1 = Matrix.random n m
        let mat2 = Matrix.random n m
        mat1 + mat2 = mat2 + mat1

    static member ``product is associative`` (n' : NonNegativeInt) (m' : NonNegativeInt) (k' : NonNegativeInt) (l' : NonNegativeInt) : bool =
        let n, m, k, l = int n', int m', int k', int l'
        let mat1 = Matrix.random n m
        let mat2 = Matrix.random m k
        let mat3 = Matrix.random k l
        (mat1 * mat2) * mat3 = mat1 * (mat2 * mat3)

    static member ``trace is cyclic`` (n' : NonNegativeInt) : bool =
        let n = int n'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random n n
        Matrix.tr (mat1 * mat2) = Matrix.tr (mat2 * mat1)

    static member ``trace commutes with tensor product`` (n' : NonNegativeInt) : bool =
        let n = int n'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random n n
        Matrix.tr (mat1 *! mat2) = (Matrix.tr (mat1) && Matrix.tr (mat2))

    static member ``rank-nullity`` (mat : Matrix) : bool =
        let numberForm = Matrix.rk mat + Matrix.nul mat = Matrix.dimCol mat
        let spaceForm = Matrix.dimCol (Matrix.ker mat) + Matrix.dimCol (Matrix.im mat) = Matrix.dimCol mat
        numberForm && spaceForm

    static member ``determinant of product is product of determinants`` (n' : NonNegativeInt) : bool =
        let n = int n'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random n n
        Matrix.det (mat1 * mat2) = (Matrix.det mat1 && Matrix.det mat2)

    static member ``determinant of tensor product is product of determinants`` (n' : NonNegativeInt) (m' : NonNegativeInt) : bool =
        let n, m = int n', int m'
        let mat1 = Matrix.random n n
        let mat2 = Matrix.random m m
        Matrix.det (mat1 *! mat2) = (Matrix.det mat1 && Matrix.det mat2)

    static member ``product with inverse is identity`` (n' : NonNegativeInt) : Property =
        let n = int n'
        let mat = Matrix.random n n
        Matrix.det mat ==> lazy
            let id = Matrix.identity n
            let inv = Matrix.inv mat
            (mat * inv = id) && (inv * mat = id)

type ComplexTests =

    static member ``closure is idempotent`` (xs : Set<Simplex>) : bool =
        let (Complex c) = Complex.closure xs
        Complex c = Complex.closure c

    static member ``closure of facets is identity`` (xs : Set<Simplex>) : bool =
        let com = Complex.closure xs
        com = (com
               |> Complex.facets
               |> Complex.closure)

    static member ``euler characteristic of chain and homology coincide`` (xs : Set<Simplex>) : bool =
        let com = xs |> Complex.closure
        let chain = com |> Complex.boundaryChain
        let homology = chain |> Chain.homology
        Chain.euler homology = Chain.euler chain

    static member ``betti is dimension of homology`` (xs : Set<Simplex>) : bool =
        let com = xs |> Complex.closure

        let betti =
            com
            |> Complex.boundaryChain
            |> Chain.betti
            |> List.ofSeq

        let homologyDims =
            com
            |> Complex.boundaryChain
            |> Chain.homology
            |> Chain.dim
            |> List.ofSeq

        betti = homologyDims

    static member ``bettis exceeds reduced bettis by one in degree zero and coincide elsewhere`` (xs : Set<Simplex>) : Property =
        Set.count xs > 1 ==> lazy
            let com = xs |> Complex.closure

            let betti =
                com
                |> Complex.boundaryChain
                |> Chain.betti
                |> List.ofSeq

            let reducedBetti =
                com
                |> Complex.reducedBoundaryChain
                |> Chain.betti
                |> List.ofSeq

            (List.head betti = 1 + List.head reducedBetti) && (List.tail betti = List.tail reducedBetti)

    static member ``boundary is nilpotent`` (xs : Set<Simplex>) =
        Complex.Empty = (xs
                         |> Complex.closure
                         |> Complex.boundary
                         |> Complex.boundary)

type SheafTests =

    static member ``coboundary chain of constant sheaf is dual to boundary chain of complex`` (xs : Set<Simplex>) : bool =
        let com = xs |> Complex.closure
        let bc = com |> Complex.boundaryChain

        let cbc =
            com
            |> Sheaf.constant
            |> Sheaf.coboundaryCochain

        let dcbc =
            cbc
            |> Chain.map ((~-))
            |> Chain

        let (Chain b) = bc
        let (Chain c) = dcbc
        Seq.toList b = Seq.toList c

    static member ``cobetti and betti numbers of complex coincide`` (xs : Set<Simplex>) : bool =
        let com = xs |> Complex.closure

        let betti =
            com
            |> Complex.betti
            |> Seq.toList

        let cobetti =
            com
            |> Sheaf.constant
            |> Sheaf.cobetti
            |> Seq.toList

        betti = cobetti

let testAll (endSize : int) : unit =
    let config = { Config.Default with EndSize = endSize }
    Check.All<MatrixTests>(config)
    Check.All<ComplexTests>(config)
    Check.All<SheafTests>(config)
