// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SudoSolver.Core

let swap (x,y) = (y,x)

let glyphsGroup5x5 =
    Map.empty
    |>Graph.groupMapAdd [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1)]
    |>Graph.groupMapAdd [(0, 3); (0, 4); (1, 3); (1, 4); (2, 4)]
    |>Graph.groupMapAdd [(2, 0); (3, 0); (3, 1); (4, 0); (4, 1)]
    |>Graph.groupMapAdd [(4, 2); (3, 3); (4, 3); (3, 4); (4, 3)]
    |>Graph.groupMapAdd [(1, 2); (2, 1); (2, 2); (2, 3); (3, 2)]

let graph5x5 =
    {
        Graph.Width = 5
        Graph.Height = 5
        Graph.PossibleElements = [1;2;3;4;5] |> Set.ofList
        Graph.GroupMap = glyphsGroup5x5
        Graph.Cells = Map.empty
    }
    |>Graph.groupRows
    |>Graph.groupColumns

[<EntryPoint>]
let main argv = 

    let cells =
        [
            (4, 3), 1
            (4, 1), 2; (0, 4), 2
            (0, 2), 3;
            (0, 0), 4
        ]
        |>List.map (fun (x, y) -> (x, Written y))
        |>Map.ofList

    let graph = {graph5x5 with Cells = cells}
    let solution = Solver.solve graph
    let rec printSoluction = function
        |graph'::tail ->
            Array2D.init 5 5 (fun x y -> graph'.Cells |> Map.find (x,y))
            |>Array2D.map (function |Written x -> x |_ -> invalidOp "Not solved")
            |>printfn "%A"
            printSoluction tail
        |[] -> printfn "End of results"
    printSoluction solution
    0 // return an integer exit code
