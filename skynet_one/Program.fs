module X


#if INTERACTIVE
#load "Shared.fs"
#load "Model.fs"
#load "GraphTools.fs"
#load "Acquisition.fs"
#load "GraphStructureAdapters.fs"
#endif 

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

let computeImpactIfEdgeRemoved graph agentSmith gateway =
    let pathData = shortestPath agentSmith gateway graph
    Path.collectAllEdgesOnPaths pathData gateway 
    |> Seq.map (fun e -> e, G.without graph e |> shortestPath agentSmith gateway)

let nextWithManyGateways graph agentSmith gateways =  
    let computeOnGateway gateway =
        shortestPath agentSmith gateway graph
    let gw = 
        Seq.map (fun g -> {| Gateway = g ; Path = computeOnGateway g |}) gateways
        |> Seq.groupBy (fun d -> Path.totalLength d.Gateway d.Path)
        |> Seq.sortBy fst
        |> Seq.tryHead 
        |> Option.map snd
    ()

let nextEdge graph agentSmith gateway =         
    computeImpactIfEdgeRemoved graph agentSmith gateway
    |> Seq.maxBy (fun (e, path) -> Path.totalLength gateway path)
    |> fst

#if INTERACTIVE

#else 

[<EntryPoint>]
let main argv =

    
    let g = [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 3) ] |> List.map Edge |> DirtyG |> G.create

    let result = shortestPath 0 3 g
    let txt = sprintf "%A" result
    //output.WriteLine txt


    printfn "%A" argv

    Console.WriteLine($"{DateTime.Now} : Building grid")
    let grid = TestData.makeGrid (if Array.isEmpty argv then 10000000 else int(argv.[0])) 
    Console.WriteLine($"{DateTime.Now} : starting")
    let grid = TestData.makeGrid 2 1000
    Console.WriteLine($"{DateTime.Now} : constructed as edge list")
    let tr = asGraph grid
    Console.WriteLine($"{DateTime.Now} : transfomed as sparse adjancency matrix")
    let e = asAdjList tr
    Console.WriteLine($"{DateTime.Now} : constructd back to edge list")
    
    let grid = 
        [ (0, 1)
          (0, 2)
          (2, 3)
          (5, 2)
          (6, 0)
          (1, 5)
          (7, 5) ] |> List.map Edge |> DirtyG |> G.create

    let test = nextEdge grid 0 3
    printfn "my choice : %A" test
    0

#endif
