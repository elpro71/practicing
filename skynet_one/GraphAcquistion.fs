module Acquisition
open System
open Shared
open GraphModel 

type Env = 
    | TestScenario0
    | TestScenario1 
    | TestScenario2
    | Prod

let read (edgeCountReader:Reader<Env,int>) (edgeReader:Reader<Env, (int * int)>) =
    let readEdges nbrLines = 
        [ 1.. nbrLines]
        |> List.map (konst edgeReader)
        |> Reader.sequence
    Reader.bind readEdges edgeCountReader       
    |> Reader.map (List.map Edge >> DirtyG)

[<AutoOpen>]
module TestData = 

    let makeGrid n =
        let unfold queue = 
            let updateQueue queue =
                let getLastIndex queue = 
                    match queue with 
                    | { List = [] ; Last = None } -> -1
                    | { Last = Some (_, y) } -> y 
                    | { List = l } -> List.last l |> snd
                let adds dest queue = 
                    let last = getLastIndex queue
                    queue |> Queue.enqueue (dest, last+1) |> Queue.enqueue (dest, last+2)
                Queue.dequeue queue 
                |> Option.map (fun (edge, q) -> edge, adds (snd edge) q)
                |> Option.defaultWith (fun _ -> failwith "invalidLogic")                                
            updateQueue queue |> Some 
        
        let queue = { List = [ (0, 1) ] ; Last = Some ( 0, 2 ) }
        Seq.unfold unfold queue
        |> Seq.take n
        |> Seq.map Edge
        |> Seq.toList
        |> DirtyG
        |> G.create


    let graphs = 
        [   [ (0,1); (0,2);(1,3); (2,4); (2,5); (3,6); (4,7); (5, 7); (6,8); (7, 8) ]
            [ (0, 1); (0, 2); (1, 2); (1, 3) ]  
            [ (0, 1); (0, 2); (1, 2); (1, 3) ] ]

    let getDirtyGraph x = 
        graphs
        |> List.item x 
        |> List.map Edge
        |> DirtyG
             
let readNbrEdge = 
        function 
            | TestScenario0 -> List.length graphs.[0]
            | TestScenario1 -> List.length graphs.[1]
            | TestScenario2 -> List.length graphs.[2]
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
        | TestScenario2 -> readDataLine 2
        | Prod -> 
            let values = 
                Console.In.ReadLine().Split(' ') 
                |> Array.map Int32.Parse
            match values with 
            | [| f ; s |] -> f,s
            | _ -> failwith " wrong ! wrong! wrong ! try again"
    |> Reader

