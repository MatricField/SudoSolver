module Complicated1Runner
open SudoSolver.Complicated

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
    }
    |>Graph.groupRows
    |>Graph.groupColumns

let main argv = 

    let cells =
        [
            (4, 3), 1
            (4, 1), 2; (0, 4), 2
            (0, 2), 3;
            (0, 0), 4
        ]
        |>Map.ofList

    let graph = graph5x5
    let solution = Solver1.solve graph cells 
    let rec printSoluction = function
        |graph'::tail ->
            Array2D.init 5 5 (fun x y -> graph' |> Map.find (x,y))
            |>printfn "%A"
            printSoluction tail
        |[] -> printfn "End of results"
    solution
    |>Array.toList
    |>printSoluction
    0 // return an integer exit code
