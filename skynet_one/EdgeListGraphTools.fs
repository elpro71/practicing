module EdgeListTools

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

let shortPaths graph origin dest = 
    let s =
        bf graph origin
        |> Seq.toList

    let getParent n = 
        let checkParent { Node = node ; Parents = parents } = 
            node = n
        List.tryFind checkParent 

    let buildPath dest = 
        let aggregate some =
            match some with
            | Some { Node = n ; Parents = (p::_) } -> getParent p s |> Option.map (fun pp -> n, Some pp)
            | _ -> None
        List.unfold aggregate (getParent dest s)

    buildPath dest


