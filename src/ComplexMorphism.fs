module ComplexMorphism

/// Convenience constructor to produce a simplicial map from its action on nodes.
/// WARNING: no check is made to check if the map is really simplicial.
let make (input : (int * int) list, source : Complex, target : Complex) : ComplexMorphism =
    let nodeMap =
        input
        |> List.map (Helpers.pairMap Label)
        |> Map.ofList
    (Simplex.map (fun v -> Map.find v nodeMap)) >> Simplex, source, target

// Inclusion morphism of complexes.
let inclusion (source : Complex, target : Complex) : ComplexMorphism = id, source, target
// Identity morphism of complexes.
let identity (com : Complex) : ComplexMorphism = inclusion (com, com)

// Chain morphism induced by a complex morphism.
let chainMorphism (mor : ComplexMorphism) : Chain =
    let func, source, target = mor

    let sourceSkeleton =
        source
        |> Complex.skelatalDecompositionList
        |> List.tail

    let targetSkeleton =
        target
        |> Complex.skelatalDecompositionList
        |> List.tail

    (Seq.map ((fun (s, t) -> Array2D.init (List.length t) (List.length s) (fun r c -> func s.[c] = t.[r])) >> Matrix) ((sourceSkeleton, targetSkeleton) ||> Seq.zip)) |> Chain

// Homology morphism induced by a complex morphism.
let homologyMorphism (mor : ComplexMorphism) : Chain =
    let _, source, target = mor
    let (Chain chainMor) = chainMorphism mor
    let (Chain homologySource) = Complex.homology source
    let homologyImage = ((chainMor, homologySource) ||> Seq.zip) |> Seq.map ((fun (c, h) -> c * h))
    let (Chain homologyTarget) = Complex.homology target
    (homologyTarget, homologyImage)
    ||> Seq.zip
    |> Seq.map ((fun (h, i) -> Matrix.linSolve h i) >> Matrix.distinctCols)
    |> Chain
