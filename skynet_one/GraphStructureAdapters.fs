module GraphAdapters 
open GraphModel
   
let (|AsAdjList|) graph =
    let makeEdgeList i l =
        let makeEdge i x = Edge (i, x)    
        l |> NodeAdjacency.unwrap |> List.map (makeEdge i)

    graph
    |> Graph.unwrap 
    |> List.mapi makeEdgeList
    |> List.concat
    |> DirtyG
    |> G.create

let asGraph = function | AsAdjGraph g -> g
let asAdjList = function | AsAdjList g -> g    