module GraphModel
open Shared

open System.Runtime.CompilerServices
[<assembly:InternalsVisibleTo("UnitTests")>]
do ()

type Edge = internal Edge of (int * int)
module Edge = let unwrap (Edge c) = c
    
type DirtyG = DirtyG of Edge list
type GraphType = 
    | FreeForm
    | DAG

type G = internal G of Edge list
type CleanEdgeList = DirtyG -> G

module G =
    let private cleanUpImp requestedType (DirtyG edges) =
        match requestedType with
        | DAG -> edges |> List.distinct |> G
        | FreeForm -> 
            let reverse (Edge edge) = (snd edge, fst edge)|> Edge
            edges @  (edges |> List.map reverse) 
            |> List.distinct 
            |> List.sort 
            |> G    

    let create : CleanEdgeList = cleanUpImp FreeForm



type NodeAdjacency = NodeAdjacency of int list
module NodeAdjacency = let unwrap (NodeAdjacency x) = x

type Graph = private Graph of NodeAdjacency list
module Graph = 
    let transform mapi (Graph nodeList) = 
        List.mapi mapi nodeList |> Graph
    let unwrap (Graph lst) = lst



let EmptyGraph size = NodeAdjacency [] |> konst |> List.init size |> Graph

let (|AsAdjGraph|) (G edges) =
    // let getMax (Edge (x, y)) = Math.Max(x,y)
    // let lastIndex = edges |> List.map getMax |> List.max
    // let update (Graph lst) edges =
    //     let updateAdjacency (edges :Edge list) index (NodeAdjacency lst) =
    //         let compEdge g index (Edge e) = g e = index               
    //         let fromEdges = edges |> List.filter (compEdge fst index) |> List.map (Edge.unwrap >> snd)                
    //         let toEdges = edges |> List.filter (compEdge snd index) |> List.map (Edge.unwrap >> fst)                
    //         lst @ fromEdges @ toEdges|> List.distinct |> NodeAdjacency 

    //     lst
    //     |> List.mapi (updateAdjacency edges)
    //     |> Graph
    //update (EmptyGraph (lastIndex+1)) edges

    let folder graph edge = 
        let update (Edge (o, d)) (Graph nodes) =
            nodes 
            |> List.mapi (fun i node -> 
                if i = o then node |> NodeAdjacency.unwrap |> ((@) [d]) |> List.distinct |> NodeAdjacency
                else if i = d then nodes.[d] |> NodeAdjacency.unwrap |> ((@) [o]) |> List.distinct |> NodeAdjacency
                else node)
        update edge graph
        |> Graph

    List.fold folder (EmptyGraph (List.length edges)) edges