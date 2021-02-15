module X


#if INTERACTIVE
#load "Shared.fs"
#load "Model.fs"
#load "GraphTools.fs"
#load "Acquisition.fs"
#load "GraphStructureAdapters.fs"
#endif 

open Common
open System
open Shared
open GraphModel
open GraphTools
open Acquisition
open GraphAdapters

let init () =
    let gr =
        read readNbrEdge readEdge
        |> Reader.run TestScenario2     
        |> G.create

    asGraph gr

let nicePrint pr =
    printfn "Node %i" pr.Node
    printfn "\tDistance %i" pr.DistanceFromOrigin
    pr.Parents
    |> List.iter (printfn "\tfrom %i")

// best next move based on one computation iteration.

let shortestPaths graph agentSmith gateways =
    let computeShortestPath gateway = 
        shortestPath agentSmith gateway graph
        
    gateways
    |> Seq.map (fun gateway -> {| Gateway = gateway ; OPath = computeShortestPath gateway |})
    |> Seq.choose (fun r -> Option.map (fun p -> {| Gateway = r.Gateway ; Path = p |}) r.OPath)
    |> Seq.groupBy (fun res -> Path.totalLength res.Gateway res.Path)
    |> Seq.sortBy fst
    |> Seq.collect snd

let computeMostDamagingEdgeOnPath graph path agentSmith gateway =
    Path.collectAllEdgesOnPaths path gateway 
    |> Seq.map (fun e -> e, G.without graph e |> shortestPath agentSmith gateway)
    |> Seq.toList

let nextWithManyGateways graph agentSmith gateways =  

    let selectBestGroup (solutions : seq<{| Gateway: int; Path: Path |}>)= 
        solutions
        |> Seq.groupBy (fun d -> Path.totalLength d.Gateway d.Path)
        |> Seq.sortBy fst
        |> Seq.tryHead

    let computeOnGateways graph = shortestPaths graph agentSmith gateways
    let toOption = function | h::t -> Some (h::t) | [] -> None

    optional {
        let solutions = computeOnGateways graph        
        let! group = selectBestGroup solutions          
        let paths = snd group
        let allPathEdges = 
            paths 
            |> Seq.collect (fun x -> Path.collectAllEdgesOnPaths x.Path x.Gateway)
            |> Seq.distinct
        let! edgeByGateWayBestPath =
            allPathEdges 
            |> Seq.map (fun edge -> edge, G.without graph edge 
                                            |> computeOnGateways
                                            |> Seq.tryHead)
            |> Seq.toList 
            |> toOption
        return
            edgeByGateWayBestPath
            |> Seq.minBy (fun res ->
                    match res with
                    | _, None -> Int32.MinValue
                    | _, Some p -> Path.totalLength p.Gateway p.Path)
            |> fst                                                    
    }
    
let nextEdge graph agentSmith gateway =         
    let path = shortestPath agentSmith gateway graph
    Option.map (fun p -> computeMostDamagingEdgeOnPath graph p agentSmith gateway
                         |> Seq.maxBy (fun (e, path) -> Path.totalLength gateway p)
                         |> fst)

#if INTERACTIVE

#else 

let printSize = G.unwrap >> List.length >> printfn "%d"

[<EntryPoint>]
let main argv =

    
    let g = [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 3) ] |> List.map Edge.create |> DirtyG |> G.create

    let result = shortestPath 0 3 g
    let txt = sprintf "%A" result
    //output.WriteLine txt


    printfn "%A" argv

    Console.WriteLine($"{DateTime.Now} : Building grid")
    let grid = TestData.makeGrid (if Array.isEmpty argv then 1000000000 else int(argv.[0])) 
    Console.WriteLine($"{DateTime.Now} : starting")
    let grid = TestData.makeGrid 2 1000
    Console.WriteLine($"{DateTime.Now} : constructed as edge list")
    let tr = asGraph grid
    Console.WriteLine($"{DateTime.Now} : transfomed as sparse adjancency matrix")
    let e = asAdjList tr
    Console.WriteLine($"{DateTime.Now} : constructd back to edge list")
    
    printSize grid
    printSize e

    
    let grid = 
        [ (0, 1)
          (0, 2)
          (2, 3)
          (5, 2)
          (6, 0)
          (1, 5)
          (7, 5) ] |> List.map Edge.create |> DirtyG |> G.create

    let test = nextWithManyGateways grid 0  [ 3; 3 ]

    printfn "my choice : %A" test
    0

#endif
