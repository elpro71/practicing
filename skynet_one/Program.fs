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
open PuzzleLogic
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

// best next move based on one computation iteration.


let printSize = G.unwrap >> List.length >> printfn "%d"

type S = { Idx : int; Count : int; TotCount : int }
module S = 
    let addZero prevIndex seq =
        if seq.Idx = -1 then        
            { Idx = prevIndex + 1; Count = 0; TotCount = seq.TotCount + 1 }
        else 
            { Idx = prevIndex + 1 ; Count = 0; TotCount = seq.Count + 1 }
                    
    let addOne sequence = { sequence with Count = sequence.Count + 1 ; TotCount = sequence.TotCount + 1 }
    let max s ns = if ns.TotCount > s.TotCount then ns else s

type State = { Idx: int ; Cur : S ; Max : S }
module State = 
    let initSeq = { Idx = -1 ; Count = 0 ; TotCount = 0 }
    let empty = { Idx = 0; Cur = initSeq; Max = initSeq }

    let addZero index current max = 
        let nIdx = index + 1
        let n = S.addZero index current
        { Idx = nIdx ; Cur = n ; Max = S.max max n }

    let addOne index current max  = 
        let c = S.addOne current        
        { Idx = index + 1 ; Cur = c ; Max = S.max c max }  

    let folder { Idx = prevIndex; Cur= current ; Max = max }  char = 
        let idx = prevIndex
        if char = '1' then 
            addOne idx current max
        else 
            addZero idx current max
       

let solve (string :String)= 
    string.ToCharArray() 
    |> (Array.fold State.folder State.empty)
    |> (fun solution -> printfn "%d" solution.Max.TotCount)



// let distance (ar1 : int array) (ar2 : int array) = abs (ar1.[0]-ar2.[0]) + abs (ar1.[1]-ar2.[1])
// let getdata () = 
//     [ 1 .. (int(Console.In.ReadLine())) ]
//     |> List.map (fun _ -> 
//         Console.In.ReadLine().Split(' ')
//         |> Array.map int)
// let data = getdata ()
// let getMin d i e = 
//     d
//     |> List.mapi (curry id)
//     |> List.filter (fst >> ((<>) i))
//     |> List.map (snd >> distance e)
//     |> List.min
// data
// |> List.mapi (fun index e -> getMin data index e)
// |> List.min
// |> (printfn "%d")


let computeRisk graph gateways fringe (pr: PathResult) = 
    let problems = 
        List.unfold 
            (fun p -> 
                    let connection = 
                        let (children,_) = G.extractEdges pr.Node graph
                        Set.intersect (Set children) (Set gateways) |> Set.count
                        
                    let getParentPr p = 
                        List.find (fun x -> x.Node = p.Parents.Head) fringe

                    if List.isEmpty pr.Parents then None
                    else  
                        let exist = List.exists (fun x -> x = p.Node) gateways 
                        if exist then 
                            Some (connection, getParentPr p)
                        else
                            None) pr
        |> List.sum
    pr.DistanceFromOrigin - problems

let computeFreeMove graph gateways fringe (node, _) =
    fringe |> List.tryFind (fun pr -> pr.Node = node)
    |> Option.map (computeRisk graph gateways fringe)
    |> Option.defaultValue Int32.MaxValue

let selectNodesToGateways (G edges) gateways = 
    edges |> List.distinct |> List.choose 
        (fun edge ->
            let (x, y) = Edge.unwrap edge
            match  Edge.unwrap edge with 
            | (x, y) when List.contains x gateways -> Some (y, edge)
            | (x, y) when List.contains y gateways -> Some (x, edge)
            | _ -> None)

let pickEdge graph smith gateways =
                
    let nodesToGateways = selectNodesToGateways graph gateways


    let fringe = computeFringe graph smith |> Seq.toList
    let comp x = 
        eprintfn "comparing %A" x 
        computeFreeMove graph gateways fringe x
    nodesToGateways
    |> List.minBy comp

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
    
    let graphWithoutGateways =
        G.select (fun edge -> 
            let t = Edge.unwrap edge
            List.exists (oneOfIs t) gateways |> not            
             ) g
    let fringe = computeFringe graphWithoutGateways agentSmith |> Seq.toList

    let (x, edge) = pickEdge g agentSmith gateways
    g <-  G.without g edge

    0


