// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SudoSolver.Core
open System

let swap (x,y) = (y,x)

let glyphsGroup5x5 =
    Map.empty
    |>Solver.groupMapAdd [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1)]
    |>Solver.groupMapAdd [(0, 3); (0, 4); (1, 3); (1, 4); (2, 4)]
    |>Solver.groupMapAdd [(2, 0); (3, 0); (3, 1); (4, 0); (4, 1)]
    |>Solver.groupMapAdd [(4, 2); (3, 3); (4, 3); (3, 4); (4, 3)]
    |>Solver.groupMapAdd [(1, 2); (2, 1); (2, 2); (2, 3); (3, 2)]

let graph5x5 =
    {
        Graph.Width = 5
        Graph.Height = 5
        Graph.PossibleElements = [1;2;3;4;5] |> Set.ofList
        Graph.GroupMap = glyphsGroup5x5
        Graph.Cells = Map.empty
    }
    |>Solver.graphGroupRows
    |>Solver.graphGroupColumns

[<EntryPoint>]
let main argv = 

    let cells =
        [
            (4, 3), 1
            (4, 1), 2; (0, 4), 2
            (0, 2), 3;
            (0, 0), 4
        ]
        |>List.map (fun (x, y) -> (x, Given y))
        |>Map.ofList

    let graph = {graph5x5 with Cells = cells}
    let solution = Solver.solve graph
    match solution with
    |Some graph' ->
        Array2D.init 5 5 (fun x y -> graph'.Cells |> Map.find (x,y))
        |>Array2D.map (function |Given x -> x |Selected x -> x |_ -> invalidOp "Not solved")
        |>printfn "%A"
    |None -> printfn "Cannot solve"
    0 // return an integer exit code
