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
let nextWithManyGateways graph agentSmith gateways =  
    failwith "notImplemented"

let next graph agentSmith gateway =         
    let pathData = shortestPath agentSmith gateway graph
    let edges = Path.collectAllEdgesOnPaths pathData gateway 
    edges 
    |> Seq.map (fun e -> e, G.without graph e |> shortestPath agentSmith gateway)
    |> Seq.maxBy (fun (e, path) -> Path.totalLength path)
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
          (0, 3)
          (1, 5)
          (2, 3)
          (3, 6) 
          (3, 7)
          (7, 5) ] |> List.map Edge |> DirtyG |> G.create

    let test = next grid 0 5 
    printfn "my choice : %A" test
    0

#endif
