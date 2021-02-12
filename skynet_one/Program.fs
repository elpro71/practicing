module X

open System
open Shared
open GraphModel
open EdgeListTools
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


let next graph agentSmith exits  : G -> Edge -> Edge list -> Edge =  
    
    failwith ""


[<EntryPoint>]
let main argv =

    printfn "%A" argv

    Console.WriteLine($"{DateTime.Now} : Building grid")
    let t = TestData.makeGrid (if Array.isEmpty argv then 10000000 else int(argv.[0])) 
    
    Console.WriteLine($"{DateTime.Now} : starting")
    let grid = TestData.makeGrid 2 1000
    Console.WriteLine($"{DateTime.Now} : constructed as edge list")
    let tr = asGraph grid
    Console.WriteLine($"{DateTime.Now} : transfomed as sparse adjancency matrix")
    let e = asAdjList tr
    Console.WriteLine($"{DateTime.Now} : constructd back to edge list")
    0
