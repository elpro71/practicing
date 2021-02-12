module EdgeListTools

open Shared
open GraphModel
type PathResult =
    { Node : int 
      DistanceFromOrigin : int 
      Parents : int list }

module PathResult =
    let createRoot n = { Node = n ; DistanceFromOrigin = 0 ; Parents = [] }
    let createChild { Node = p ; DistanceFromOrigin = n }  c = { Node = c ; DistanceFromOrigin = n+1;  Parents  = [p] }
    let addParent { Node = parent ; } child = { child with Parents = child.Parents @ [parent] }
    let sameNode { Node = p } { Node = q } = p = q            

type Path = private Path of PathResult list
module Path =
    let create root = [ PathResult.createRoot root ] |> Path
    let skip s (Path p) = List.skip s p |> Path

let merge (Path path) children =
    let reduce children k = 
        let search = List.tryFindIndex (fun x -> x.Node = k.Node) children
        match search with
        | Some index when k.DistanceFromOrigin = children.[index].DistanceFromOrigin -> PathResult.addParent k children.[index]
        | _ -> k
    let reducedPath = path |> List.map (reduce children) 
    let missingElements = 
        children 
        |> List.filter (fun c -> List.exists (PathResult.sameNode c) reducedPath |> not)
    reducedPath @ missingElements |> Path

let bf graph p =
    let lst = Graph.unwrap graph
    let rec bF buf beginOfq =
        seq {
            let sbuf = Path.skip beginOfq buf
            match sbuf with
            | Path (c::_) -> 
                yield c
                let (NodeAdjacency children) = lst.[c.Node]
                let cpath = children |> List.map (PathResult.createChild c)
                yield! bF (merge buf cpath) (beginOfq+1)
            | _ -> yield! [] }
    bF (Path.create p) 0


let bfEdges g p =
    let rec bF buf beginOfq  g =
        seq {
            let sbuf = Path.skip beginOfq buf
            match sbuf with
            | Path (c::_) -> 
                yield c
                let (edges, g') = G.extractEdges p g
                let children = List.collect (Edge.unwrap >> tupleToList) edges |> List.distinct                
                let cpath = children |> List.map (PathResult.createChild c)
                yield! bF (merge buf cpath) (beginOfq+1) g'
            | _ -> yield! [] }
    bF (Path.create p) 0 g


let shortestPath graph origin dest = 
    let bfComputation =
        bfEdges graph origin
        |> Seq.pairwise

    bfComputation


let shortestPathx graph origin dest = 
    let bfComputation =
        bfEdges graph origin
        |> Seq.pairwise
        |> Seq.takeWhile (fun t -> (fst t).Node = dest |> not)
        |> Seq.map snd
        |> Seq.toList

    let getParent n = 
        let checkParent { Node = node ; Parents = parents } = 
            node = n
        List.tryFind checkParent 

    let aggregate some =
        match some with
        | Some { Node = n ; Parents = (p::_) } -> getParent p bfComputation |> Option.map (fun pp -> n, Some pp)
        | _ -> None
    List.unfold aggregate (getParent dest bfComputation)
    


