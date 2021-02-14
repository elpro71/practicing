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
type SanitizeEdges = DirtyG -> G
type TrySanitizeEdges = DirtyG -> Result<G, string>

module G =
    let private cleanUpImp requestedType (DirtyG edges) =
        let minimalFilter = List.map Edge.unwrap >> List.distinct >> List.map Edge

        match requestedType with
        | DAG -> edges |> minimalFilter |> G
        | FreeForm ->
            let reverse (Edge edge) = (snd edge, fst edge)|> Edge
            edges @  (edges |> List.map reverse)
            |> minimalFilter
            |> List.filter (fun (Edge (x, y)) -> x<>y)
            |> List.sort
            |> G

    let create : SanitizeEdges = cleanUpImp FreeForm

    let extractEdges parent (G edges) = 
        let edgeChildren, remainingEdges = List.partition (fun (Edge (o, d)) -> parent = o || parent = d) edges
        let children = List.collect (Edge.unwrap >> tupleToList) edgeChildren |> List.filter ((<>) parent) |> List.distinct        
        children, (G remainingEdges)

    let without (G edges) (Edge edge) =         
        edges |> List.filter (fun (Edge t) -> edge <> t && edge <> tupleFlip t) |> G

type NodeAdjacency = NodeAdjacency of int list
module NodeAdjacency = let unwrap (NodeAdjacency x) = x

type Graph = private Graph of NodeAdjacency list
module Graph = 
    let transform mapi (Graph nodeList) = 
        List.mapi mapi nodeList |> Graph
    let unwrap (Graph lst) = lst

let EmptyGraph size = NodeAdjacency [] |> konst |> List.init size |> Graph

     