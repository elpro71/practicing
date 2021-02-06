module Acquisition

open System
open Shared
open GraphModel 

type Env = 
    | TestScenario0
    | TestScenario1 
    | Prod

let read (edgeCountReader:Reader<Env,int>) (edgeReader:Reader<Env, (int * int)>) =
    let readEdges nbrLines = 
        [ 1.. nbrLines]
        |> List.map (konst edgeReader)
        |> Reader.sequence
    Reader.bind readEdges edgeCountReader       
    |> Reader.map (List.map Edge >> G)

[<AutoOpen>]
module TestData = 
    let graphs = [
             [ (0,1); (0,2);(1,3); (2,4); (2,5); (3,6); (4,7); (5, 7); (6,8); (7, 8) ]
             [ (0, 1); (0, 2); (1, 2); (1, 3) ]    
    ]



let readNbrEdge = 
        function 
            | TestScenario0 -> List.length graphs.[0]
            | TestScenario1 -> List.length graphs.[1]
            | Prod -> Int32.Parse(Console.In.ReadLine())
        |> Reader
   
let readEdge =
    let mutable currentLine = 0

    let readDataLine scenario = 
        let index = currentLine         
        currentLine <- currentLine + 1
        graphs.[scenario].[index]

    fun env ->
        match env with
        | TestScenario0 -> readDataLine 0         
        | TestScenario1 -> readDataLine 1                
        | Prod -> 
            let values = 
                Console.In.ReadLine().Split(' ') 
                |> Array.map Int32.Parse
            match values with 
            | [| f ; s |] -> f,s
            | _ -> failwith " wrong ! wrong! wrong ! try again"
    |> Reader





