/// Miscellaneous helper functions.
[<RequireQualifiedAccess>]
module Helpers

/// Map a function to a homogeneous pair.
let internal pairMap (f : 'a -> 'b) (x : 'a, y : 'a) : 'b * 'b = f x, f y

/// Number of distinct list lengths in a list of lists.
let internal listNumDistinctListLengths (ll : 'a list list) : int =
    ll
    |> List.map List.length
    |> List.distinct
    |> List.length

/// Tries to find a specified element in an 2D array. Maybe returns the coordinate.
let internal array2DtryFind (x : 'a) (a : 'b [,]) : (int * int) option =
    let len = Array2D.length2 a
    a
    |> Seq.cast
    |> Seq.mapi (fun i y -> (i / len, i % len), y)
    |> Seq.tryFind (fun (_, y) -> y = x)
    |> fun r ->
        match r with
        | Some r -> Some(fst r)
        | None -> None

/// Powerset of a set.
let rec internal powerset (s : 'a Set) : 'a Set Set =
    set [ yield s
          for e in s do
              yield! powerset (Set.remove e s) ]

/// Set of k-subsets of a set.
let internal kSubsets (k : int) (set : 'a Set) : 'a Set Set = powerset set |> Set.filter (fun s -> k = Set.count s)

/// Symmetric difference of two sets.
let internal symmetricDifference (xs1 : 'a Set) (xs2 : 'a Set) : 'a Set = (xs1 - xs2) + (xs2 - xs1)

/// Debug print.
let internal debugX x =
    printfn "%A" x
    x
