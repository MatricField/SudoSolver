namespace SudoSolver.Core

[<Struct>]
type Cell<'T when 'T : comparison> =
    |Written of Element: 'T
    |Empty of PossibleElement: Set<'T>

type Graph<'T when 'T : comparison> ={
    Width: int
    Height: int
    PossibleElements: Set<'T>
    GroupMap: Map<(int*int), Set<(int*int)>>
    Cells: Map<int * int, Cell<'T>>
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