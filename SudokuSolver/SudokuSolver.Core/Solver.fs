namespace SudoSolver.Core

type Graph<'T when 'T : comparison> ={
    Width: int
    Height: int
    PossibleElements: Set<'T>
    GroupMap: Map<(int*int), Set<(int*int)>>
}

module Graph =
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

    let groupColumns graph =
        let {Width = width; Height = height; GroupMap = groupMap} = graph
        let groupMap' =
            Seq.init width ((fun y -> Seq.init height (fun x -> (x, y))) >> Set.ofSeq)
            |>Seq.fold addToGroupMap groupMap
        {graph with GroupMap = groupMap'}

    let groupRows graph =
        let {Width = width; Height = height; GroupMap = groupMap} = graph
        let groupMap' =
            Seq.init height ((fun x -> Seq.init width (fun y -> (x, y))) >> Set.ofSeq)
            |>Seq.fold addToGroupMap groupMap
        {graph with GroupMap = groupMap'}

    let group newGroup graph =
        let {GroupMap = groupMap} = graph
        let groupMap' =
            groupMap
            |>groupMapAdd newGroup
        {graph with GroupMap = groupMap'}

module Solver =
    let solve graph filledMap =
        let { GroupMap = groupMap } = graph

        let rec solve filledMap unusedMap =
            if unusedMap |> Map.isEmpty then
                Some [|filledMap|]
            else
                let (indexOfInterest, availableElements) =
                    unusedMap
                    |>Map.toSeq
                    |>Seq.minBy (snd >> Set.count)
                if availableElements |> Seq.isEmpty then
                    None
                else
                    let dependentCellIndeces = groupMap |> Map.find indexOfInterest
                    let fixElement element =
                        let removeFixedElement map idx =
                            match map |> Map.tryFind idx with
                            |None -> map
                            |Some availableElements ->
                                let availableElements' =
                                    availableElements
                                    |> Set.remove element
                                map |> Map.add idx availableElements'
                        let unusedMap' =
                            dependentCellIndeces 
                            |>Set.fold removeFixedElement unusedMap
                            |>Map.remove indexOfInterest
                        let filledMap' =
                            filledMap |> Map.add indexOfInterest element
                        (filledMap', unusedMap')
                    availableElements
                    |>Set.toArray
                    |>Array.Parallel.choose (fun elm -> fixElement elm ||> solve)
                    |>Array.collect id
                    |>Some

        let unusedMap =
            let
                {
                    Height = height
                    Width = width
                    PossibleElements = possibleElements
                } = graph
            [0..width-1]
            |>List.fold (fun map x ->
                [0..height-1] 
                |>List.fold (fun map y ->
                    if filledMap |> Map.containsKey (x,y) then
                        map
                    else
                        map |> Map.add (x,y) possibleElements
                    ) map
                ) Map.empty
        match solve filledMap unusedMap with
        |Some result -> result
        |None -> [||]