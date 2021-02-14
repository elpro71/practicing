module GraphTools

open Shared
open GraphModel
type PathResult =
    { Node : int 
      DistanceFromOrigin : int 
      Parents : int list }

module PathResult =
    let createRoot n = { Node = n ; DistanceFromOrigin = 0 ; Parents = [] }
    let createChild { Node = p ; DistanceFromOrigin = n }  c = { Node = c ; DistanceFromOrigin = n+1;  Parents  = [p] }
    let mergeParents { Node = _; Parents = parents } child = { child with Parents = child.Parents @ parents }
    let sameNode { Node = p } { Node = q } = p = q            

type Path = private Path of PathResult list

module Path =

    let createEmpty () = Path []

    let create root = [ PathResult.createRoot root ] |> Path

    let getDetails (Path pathResults) node = pathResults |> List.tryFind (fun x -> x.Node = node)

    let totalLength target (Path pathResults) = 
        pathResults 
        |> List.tryLast 
        |> Option.map (fun p -> if p.Node = target then p.DistanceFromOrigin else System.Int32.MaxValue) 
        |> Option.defaultValue System.Int32.MaxValue    

    let rec collectAllEdgesOnPaths pathData node = 
        seq { 
            let pathResult = getDetails pathData node        
            match pathResult with
            | Some result -> 
                let makeEdge p = Edge (p, node)
                for p in result.Parents do
                    yield makeEdge p
                    yield! collectAllEdgesOnPaths pathData p                
            | _ -> yield! []
        }

    let merge (Path path) parent children =
        let wchildren = children |> List.map (PathResult.createChild parent) 
        let (added, modified, _) =  diffBy (fun n-> n.Node) path wchildren

        let updated = 
            let choose pathElement = 
                let comp x = PathResult.sameNode pathElement x                     
                let upd index = PathResult.mergeParents pathElement wchildren.[index]
                List.tryFindIndex comp wchildren
                |> Option.map upd 
            Seq.choose choose modified
        updated |> Seq.append added |> Seq.toList |> Path

    let dequeue = function 
        | Path (parent::tail) ->  Some (parent, Path tail)
        | _ -> None


let bfEdges graph p =
    let rec bF pathSolution g =
        seq {
            let someDequeued = Path.dequeue pathSolution
            match someDequeued with
            | Some (parent, remaingPath)  -> 
                yield parent
                let (children, gWithoutC) = G.extractEdges parent.Node g                        
                yield! bF (Path.merge remaingPath parent children) gWithoutC
            | None -> yield! [] }
    bF (Path.create p) graph


let shortestPath origin dest graph = 
    bfEdges graph origin
    |> Seq.pairwise
    |> Seq.takeWhile (fun t -> (fst t).Node <> dest)
    |> Seq.map snd // unpairwise
    |> Seq.toList
    |> Path