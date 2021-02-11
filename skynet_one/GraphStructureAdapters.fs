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
    |> G