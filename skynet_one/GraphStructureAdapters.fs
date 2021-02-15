module GraphAdapters 
open GraphModel
   
let (|AsAdjGraph|) (G edges) =
    let folder graph edge = 
        let update edge graph =
            let (o, d) = Edge.unwrap edge
            Graph.transform 
                (fun i node -> 
                    if i = o then node |> NodeAdjacency.unwrap |> ((@) [d]) |> List.distinct |> NodeAdjacency
                    else if i = d then node |> NodeAdjacency.unwrap |> ((@) [o]) |> List.distinct |> NodeAdjacency
                    else node)
                graph
        update edge graph
    List.fold folder (EmptyGraph (List.length edges)) edges

let (|AsAdjList|) graph =
    let makeEdgeList i l =
        let makeEdge x = Edge.create (i, x)    
        l |> NodeAdjacency.unwrap |> List.map makeEdge

    graph
    |> Graph.unwrap 
    |> List.mapi makeEdgeList
    |> List.concat
    |> DirtyG
    |> G.create

let asGraph = function | AsAdjGraph g -> g
let asAdjList = function | AsAdjList g -> g   