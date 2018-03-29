namespace SudoSolver.Core

type Cell<'T when 'T : comparison> =
    |Given of Element: 'T
    |Selected of Element: 'T
    |Available of PossibleElement: Set<'T>

type Graph<'T when 'T : comparison> ={
    Width: int
    Height: int
    PossibleElements: Set<'T>
    GroupMap: Map<(int*int), Set<(int*int)>>
    Cells: Map<int * int, Cell<'T>>
}

module Solver =
    let solve graph =
        let 
            {
                Width = width;
                Height = height;
                PossibleElements = possibleElements;
                GroupMap = groupMap
                Cells = cells
            } = graph
        let countAvailableElements (Available elms) =
            elms |> Set.count

        let rec solve cellMap = function
            |[] -> Some cellMap
            |availableIndeces ->
                let getCell idx = cellMap |> Map.find idx

                let indexcesOfInterest =
                    availableIndeces
                    |>List.sortBy (getCell >> countAvailableElements)

                let index::tail = indexcesOfInterest

                match cellMap |> Map.find index with
                |Available possibleElements ->
                    let iterElement element =
                        let dependentCellIndex =
                            groupMap
                            |>Map.find index

                        let removeElementFromDependentCells cellMap dependentCellIndex =
                            let dependentCell =
                                cellMap |> Map.find dependentCellIndex
                            match dependentCell with
                            |Available possibleElements ->
                                let dependentCell' =
                                    possibleElements
                                    |>Set.remove element
                                    |>Available
                                cellMap
                                |>Map.add dependentCellIndex dependentCell'
                            |_ -> cellMap

                        let cellMap' =
                            dependentCellIndex
                            |>Set.fold removeElementFromDependentCells cellMap
                            |>Map.add index (Selected element)
                                
                        solve cellMap' tail
                    possibleElements
                    |>Array.ofSeq
                    |>Array.Parallel.choose iterElement
                    |>Array.tryHead
                |_ -> invalidOp "logical error"
        let indeces =
            Seq.init width (fun x -> Seq.init height (fun y -> (x,y)))
            |>Seq.collect id
            |>Set.ofSeq
        let givenCellIndeces =
            cells |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        let mapIndecesToCell index =
            match cells |> Map.tryFind index with
            |Some cell -> (index, cell)
            |None ->
                let groupMember =
                    groupMap
                    |> Map.tryFind index
                    |>Option.defaultValue Set.empty
                let unavailableElements =
                    Set.intersect groupMember givenCellIndeces
                    |>Seq.map ((fun idx -> cells |> Map.find idx) >> (fun (Given elm) -> elm))
                    |>Set.ofSeq
                (index, Available (possibleElements - unavailableElements))
        let cellMap =
            indeces
            |>Seq.map mapIndecesToCell
            |>Map.ofSeq
        match solve cellMap (indeces - givenCellIndeces |> Set.toList)  with
        |Some cellMap' -> Some {graph with Cells = cellMap'}
        |None -> None

    let addToGroupMap groupMap newGroup =
        let newGroupSet =
            newGroup
            |>Set.ofSeq
        let addGroupToGroupMap groupMap idx =
            let newGroupMembers = 
                newGroupSet |> Set.remove idx
            let oldGroupMembers =
                groupMap
                |>Map.tryFind idx
                |>Option.defaultValue Set.empty
            groupMap
            |>Map.add idx (oldGroupMembers + newGroupMembers)
        newGroup
        |>Seq.fold addGroupToGroupMap groupMap

    let inline groupMapAdd newGroup groupMap = addToGroupMap groupMap newGroup

    let graphGroupColumns graph =
        let {Width = width; Height = height; GroupMap = groupMap} = graph
        let groupMap' =
            Seq.init width ((fun y -> Seq.init height (fun x -> (x, y))) >> Set.ofSeq)
            |>Seq.fold addToGroupMap groupMap
        {graph with GroupMap = groupMap'}

    let graphGroupRows graph =
        let {Width = width; Height = height; GroupMap = groupMap} = graph
        let groupMap' =
            Seq.init height ((fun x -> Seq.init width (fun y -> (x, y))) >> Set.ofSeq)
            |>Seq.fold addToGroupMap groupMap
        {graph with GroupMap = groupMap'}

    let graphGroup newGroup graph =
        let {GroupMap = groupMap} = graph
        let groupMap' =
            groupMap
            |>groupMapAdd newGroup
        {graph with GroupMap = groupMap'}