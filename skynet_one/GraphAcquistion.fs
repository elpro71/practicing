module Acquisition
open System
open Shared
open GraphModel 

open System.Collections.Generic

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

let addTrace text x =
    printfn "%A %s "  DateTime.Now text
    x

[<AutoOpen>]
module TestData = 

    let makeGrid children n =
        let firstGen = [ 1 .. children - 1 ] |> List.map (fun x -> 0, x)
        let queue = { Queue = Queue(firstGen) ; Last = Some (0, children) }

        let generate _ = 
            let getLastIndex queue =  
                match queue.Queue.Count, queue.Last with 
                | _, Some x -> snd x
                | 0, None -> -1
                | _, _ -> 
                    queue.Queue.ToArray().[queue.Queue.Count-1] |> snd
            let updateQueue () =
                let origin = queue.Dequeue () |> Option.defaultValue (0, 0)
                let last = getLastIndex queue
                for child in [ last .. (last+children) ] do
                    queue.Enqueue (snd origin, child) |> ignore                
                origin
            updateQueue() 
        
        Seq.initInfinite generate 
        |> Seq.take n
        |> Seq.map Edge
        |> Seq.toList
        |> DirtyG
        |> addTrace "DirtyG built"
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

