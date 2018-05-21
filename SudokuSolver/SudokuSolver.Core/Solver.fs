module SudoSolver.Core.Solver

let solve graph =
    let 
        {
            Width = width;
            Height = height;
            PossibleElements = possibleElements;
            GroupMap = groupMap
            Cells = cells
        } = graph
    let countAvailableElements (Empty elms) =
        elms |> Set.count

    let rec solve cellMap availableIndeces =
        let getCell idx = cellMap |> Map.find idx
        let indexcesOfInterest =
            availableIndeces
            |>List.sortBy (getCell >> countAvailableElements)
        match indexcesOfInterest with
        // Recursive base case: All cells filled -> successed
        |[] -> [cellMap]
        |index::tail ->
            match getCell index with
            |Empty possibleElements ->
                let iterElement element =
                    let removeElementFromDependentCells cellMap dependentCellIndex =
                        let dependentCell =
                            cellMap |> Map.find dependentCellIndex
                        match dependentCell with
                        |Empty possibleElements ->
                            let dependentCell' =
                                possibleElements
                                |>Set.remove element
                                |>Empty
                            cellMap
                            |>Map.add dependentCellIndex dependentCell'
                        |_ -> cellMap
                    let cellMap' =
                        groupMap
                        |>Map.find index
                        |>Set.fold removeElementFromDependentCells cellMap
                        |>Map.add index (Written element)
                                
                    match solve cellMap' tail with
                    |[] -> None
                    |arr -> Some arr
                // Recursive base case: possibleElements is empty -> failed
                possibleElements
                |>Array.ofSeq
                |>Array.Parallel.choose iterElement
                |>Seq.collect id
                |>List.ofSeq
            |_ -> invalidOp "logical error"

    let indeces =
        Seq.init width (fun x -> Seq.init height (fun y -> (x,y)))
        |>Seq.collect id
        |>Set.ofSeq

    let givenCellIndeces =
        cells
        |>Map.toSeq |>Seq.map fst |>Set.ofSeq

    let addIndexToMap map index =
        if givenCellIndeces |> Set.contains index then
            map
        else
            let unavailableElements =
                Set.intersect givenCellIndeces (groupMap |> Map.tryFind index |> Option.defaultValue Set.empty)
                |>Seq.choose (cells.TryFind >> Option.bind (function |Written elm -> Some elm |_ -> None))
                |>Set.ofSeq
            map
            |>Map.add index (Empty (possibleElements - unavailableElements))
    let cellMap =
        indeces
        |>Seq.fold addIndexToMap cells
    match solve cellMap (indeces - givenCellIndeces |> Set.toList)  with
    |[] -> []
    |cellMaps -> cellMaps |> List.map (fun cellMap' -> {graph with Cells = cellMap'})