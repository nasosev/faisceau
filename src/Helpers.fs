/// Miscellaneous helper functions.
[<RequireQualifiedAccess>]
module Helpers

/// Number of distinct list lengths in a list of lists.
let internal listNumDistinctListLengths (ll : List<List<'a>>) =
    ll
    |> List.map List.length
    |> List.distinct
    |> List.length

/// Tries to find a specified element in an 2D array. Maybe returns the coordinate.
let internal array2DtryFind (x : 'a) (a : 'b [,]) : (int * int) option =
    a
    |> Seq.cast
    |> Seq.mapi (fun i y -> ((i / (Array2D.length2 a), i % (Array2D.length2 a)), y))
    |> Seq.tryFind (fun (_, y) -> y = x)
    |> fun r ->
        match r with
        | Some r -> Some(fst r)
        | None -> None

/// Powerset of a set.
let rec internal powerset (s : Set<'a>) : Set<Set<'a>> =
    set [ yield s
          for e in s do
              yield! powerset (Set.remove e s) ]

/// Set of k-subsets of a set.
let internal kSubsets (k : int) (set : Set<'a>) : Set<Set<'a>> = powerset set |> Set.filter (fun s -> k = Set.count s)

/// Debug print.
let debugX x =
    printfn "%A" x
    x
