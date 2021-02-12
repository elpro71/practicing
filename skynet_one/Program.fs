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


[<EntryPoint>]
let main argv =

    printfn "%A" argv

    Console.WriteLine($"{DateTime.Now} : Building grid")
    let t = TestData.makeGrid (if Array.isEmpty argv then 10000 else int(argv.[0])) 
    
    Console.WriteLine($"{DateTime.Now} : starting")
    let grid = TestData.makeGrid 1000
    Console.WriteLine($"{DateTime.Now} : constructed")
    let tr = asGraph grid
    Console.WriteLine($"{DateTime.Now} : constructed")
    let e = asAdjList tr
    Console.WriteLine($"{DateTime.Now} : constructed")
    0
