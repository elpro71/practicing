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

    let t = TestData.getDirtyGraph 1 
    let grid = TestData.makeGrid 10
    let myG = init() 
    ()

//     bf myGraph 0 
//     |> Seq.toList
//     |> Seq.iter nicePrint
    0 // return an integer exit code