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

module EdgeX =
    let oneNodeIs n edge = 
        let tupleOneOfIs n (x, y) = x = n || y =n
        edge |> Edge.unwrap |> tupleOneOfIs n

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

let printSize = G.unwrap >> List.length >> printfn "%d"

let selectNodesToGateways (G edges) gateways = 
    edges 
    |> List.distinct |> List.choose 
        (fun edge ->
            match  Edge.unwrap edge with 
            | (x, y) when List.contains x gateways -> Some (y, edge)
            | (x, y) when List.contains y gateways -> Some (x, edge)
            | _ -> None)
    |> List.groupBy fst
    |> List.map (fun (x, l) -> x, List.map snd l)


let a = 10
let b = 3
let s = 100

let soluce =
    [ for x in 1 .. s do
      for y in 1 .. s do if x * a + y * b = s then yield (x,y) ]

let pickEdge graph smith gateways = 
    let graphWithoutGateways =
        G.select (fun edge -> 
            let t = Edge.unwrap edge
            List.exists (oneOfIs t) gateways |> not            
             ) graph
    let fringe = computeFringe graphWithoutGateways smith |> Seq.toList
    let p = Path fringe
    let gs = Set gateways
    let pickNodeLinkedToGateways pr = 
        let childrenSet = G.extractEdges pr.Node graph |> fst |> Set
        let intersect = Set.intersect childrenSet gs
        if Set.isEmpty intersect then None 
        else Edge.create (pr.Node, intersect |> Set.toSeq |> Seq.item 0) |> Some

    let mEdges =
        selectNodesToGateways graph gateways
        |> List.filter (fun (_, l) -> List.length l > 1)

    let multiLinkedNode = 
        mEdges      
        |> List.minBy (fun (x,_) -> Path.getDetails p x |> Option.map (fun x -> x.DistanceFromOrigin) |> Option.defaultValue Int32.MaxValue)
        |> snd
        |> List.head 

    let emergency = 
        fringe
        |> Seq.takeWhile (fun pr -> pr.DistanceFromOrigin < 1)
        |> Seq.choose pickNodeLinkedToGateways
        |> Seq.tryHead

    Option.iter (fun edge -> eprintfn "\t\tfound emergency : %A"  edge) emergency
    Option.defaultWith (konst multiLinkedNode) emergency




[<EntryPoint>]
let main argv =

    let mutable g = 
        [   (6, 11)
            (0, 9)
            (1, 2)
            (0, 1)
            (1, 10)
            (5, 11)
            (2, 3)
            (4, 5)
            (8, 9)
            (6, 7)
            (7, 8)
            (0, 6)
            (3, 4)
            (0, 2)
            (7, 11)
            (0, 8)
            (0, 4)
            (9, 10)
            (0, 5)
            (0, 7)
            (0, 3)
            (0, 10)
            (5, 6)
            (11, 6)
            (9, 0)
            (2, 1)
            (1, 0)
            (10, 1)
            (11, 5)
            (3, 2)
            (5, 4)
            (9, 8)
            (7, 6)
            (8, 7)
            (6, 0)
            (4, 3)
            (2, 0)
            (11, 7)
            (8, 0)
            (4, 0)
            (10, 9)
            (5, 0)
            (7, 0)
            (3, 0)
            (10, 0)
            (6, 5) 
            ]
        |> List.map Edge.create |> DirtyG |> G.create
    let gateways = [ 6; 7]       
    let agentSmith = 0
    

    let edge = pickEdge g agentSmith gateways
    g <-  G.without g edge

    0


