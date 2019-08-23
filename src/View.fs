/// This module plots simplicial complexes or sheaves using XPlot.Plotly.
///
/// TODO:
/// 1. Use `layer="below"` in createTriangle.
/// 2. Use `Labels` in scatter/shapes for hover info.
///
[<RequireQualifiedAccess>]
module View

open XPlot.Plotly

[<Literal>]
let NodeSize = 20

[<Literal>]
let EdgeWidth = 5

/// Creates scatter plot of nodes.
let createNodes (coords : List<int * int>) : Scatter =
    let x, y = coords |> List.unzip
    Scatter(x = x, y = y, mode = "markers", marker = Marker(color = "black", size = NodeSize))

/// Creates edge shape.
let createEdge (coords : List<int * int>) : Shape =
    let x0, y0, x1, y1 = fst coords.[0], snd coords.[0], fst coords.[1], snd coords.[1]
    Shape(``type`` = "line", x0 = x0, y0 = y0, x1 = x1, y1 = y1, line = Line(color = "black", width = EdgeWidth))

/// Creates triangle shape.
let createTriangle color (coords : List<int * int>) : Shape =
    let createSvgPath coords =
        coords
        |> List.map (fun (x, y) -> string x + " " + string y)
        |> String.concat " L "
        |> fun str -> "M " + str + " Z"
    Shape(``type`` = "path", path = createSvgPath coords, fillcolor = color, line = Line(width = 0))

/// Plots simplicial complex with coordinates.
let complex (com : Complex) (coords : List<int * int>) : PlotlyChart =
    let labels = [ Nat.Zero..(Complex.skeletonSize 0 com - Nat.One) ] |> List.map (fun (Nat n) -> Label n)

    let coordMap =
        (labels, coords)
        ||> List.zip
        |> Map.ofList

    let simplicesToCoords (simplices : List<Simplex>) =
        simplices
        |> List.map (fun (Simplex s) ->
               s
               |> Set.toList
               |> List.map (fun point -> coordMap.[point]))

    // Nodes.
    let nodes =
        com
        |> Complex.skeleton 0
        |> Set.toList

    let nodeCoords =
        nodes
        |> simplicesToCoords
        |> List.map (fun simplex -> simplex.[0])

    let nodeChart = createNodes nodeCoords

    // Edges.
    let edges =
        com
        |> Complex.skeleton 1
        |> Set.toList

    let edgesCoords = edges |> simplicesToCoords
    let edgeShapes = edgesCoords |> List.map createEdge

    // Higher simplices.
    let higherSimplices =
        let (Complex c) = com
        c
        |> Set.filter (fun simplex -> 1 < Simplex.dim simplex)
        |> Complex
        |> Complex.facets
        |> Set.toList

    let triangleShapes color (triangles : List<Simplex>) =
        triangles
        |> simplicesToCoords
        |> List.map (createTriangle color)

    let simplexColor s =
        let dim = Set.count s
        let red = 2 * 255 / (dim - 1)
        let blue = 255 - red
        sprintf "rgba(%A, 0, %A, .5)" red blue

    let triangleShapes =
        (List.map ((fun (Simplex s) ->
                   (s |> simplexColor,
                    s
                    |> Helpers.kSubsets 3
                    |> Set.map Simplex
                    |> Set.toList))
                   >> (fun (color, triangleList) -> triangleShapes color triangleList)) higherSimplices)
        |> List.collect id

    // Consolidate and plot.
    let shapes = edgeShapes @ triangleShapes
    let layout = Layout(shapes = shapes, title = string com)
    seq [ nodeChart ]
    |> Chart.Plot
    |> Chart.WithLayout layout

/// Plot simplicial complex with random coordinates.
let complexRng (com : Complex) : PlotlyChart =
    let rng = System.Random()
    let next() = rng.NextDouble() * 100.0 |> int

    let randomCoords =
        [ for i in Nat.Zero..(Complex.skeletonSize 0 com - Nat.One) -> next(), next() ]
    complex com randomCoords
