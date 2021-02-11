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
        |> cleanUp

    let asGraph = function | AsAdjGraph g -> g
    let asAdjList = function | AsAdjList g -> g
    asGraph gr


let nicePrint pr =
    printfn "Node %i" pr.Node
    printfn "\tDistance %i" pr.DistanceFromOrigin
    pr.Parents
    |> List.iter (printfn "\tfrom %i")


[<EntryPoint>]
let main argv =

    let grid = TestData.makeGrid 10 |> Seq.toList 


    let myG = init() 

    ()

//     bf myGraph 0 
//     |> Seq.toList
//     |> Seq.iter nicePrint
    0 // return an integer exit code