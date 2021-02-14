module X


#if INTERACTIVE
#load "Shared.fs"
#load "Model.fs"
#load "GraphTools.fs"
#load "GraphAcquisition.fs"
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
let next graph agentSmith gateways =  
    let computePaths g = 
        let search = shortestPath agentSmith 
        Seq.map ((flip search g) >> Seq.toList >> (fun x -> x, List.length x)) gateways |> Seq.toList

    let paths = computePaths graph
    let minLength = paths |> Seq.minBy snd |> snd
    let edges = 
        Seq.collect fst (paths 
        |> Seq.filter (snd >> (=) minLength))
        |> Seq.toList

    edges 
    |> List.map (fun e -> e, G.without graph e |> computePaths |> Seq.minBy snd |> snd)
    |> List.maxBy snd
    |> fst

#if INTERACTIVE

#else 

[<EntryPoint>]
let main argv =

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
    
    let test = next grid 0 [ 10 ] 
    0

#endif
