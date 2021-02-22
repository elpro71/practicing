module PuzzleLogic

open System
open Common
open GraphModel
open GraphTools


let shortestPaths graph agentSmith gateways =
    let computeShortestPath gateway = 
        shortestPath agentSmith gateway graph
        
    gateways
    |> Seq.map (fun gateway -> {| Gateway = gateway ; OPath = computeShortestPath gateway |})
    |> Seq.choose (fun r -> Option.map (fun p -> {| Gateway = r.Gateway ; Path = p |}) r.OPath)
    |> Seq.groupBy (fun res -> Path.totalLength res.Gateway res.Path)
    |> Seq.sortBy fst
    |> Seq.collect snd

// let computeAllPathsWithEdgeExtracted graph path agentSmith gateway =
//     Path.collectAllEdgesOnPaths path gateway 
//     |> Seq.map (fun e -> e, G.without graph e |> shortestPath agentSmith gateway)
//     |> Seq.toList

let nextWithManyGateways graph agentSmith gateways =  
    let tupleOneOf v (x, y) = v = x || v = y

    let selectBestGroup (solutions : seq<{| Gateway: int; Path: Path |}>)= 
        solutions
        |> Seq.groupBy (fun d -> Path.totalLength d.Gateway d.Path)
        |> Seq.sortBy fst
        |> Seq.tryHead

    let computeOnGateways graph = shortestPaths graph agentSmith gateways
    let toOption = function | h::t -> Some (h::t) | [] -> None

    optional {
        let solutions = computeOnGateways graph        
        let! group = selectBestGroup solutions          
        let paths = snd group
        let allPathEdges = 
            paths 
            |> Seq.collect (fun x -> Path.collectAllEdgesOnPaths x.Path x.Gateway)
            |> Seq.distinct
        return allPathEdges
        |> Seq.find (fun edge -> List.exists (fun gateway -> edge |> Edge.unwrap |> (tupleOneOf gateway)) gateways)
        // let! edgeByGateWayBestPath =
        //     allPathEdges 
        //     |> Seq.map (fun edge -> edge, G.without graph edge 
        //                                     |> computeOnGateways
        //                                     |> Seq.tryHead)
        //     |> Seq.toList 
        //     |> toOption
        // return
        //     edgeByGateWayBestPath
        //     |> Seq.maxBy (fun res ->
        //             match res with
        //             | _, None -> Int32.MaxValue
        //             | _, Some p -> Path.totalLength p.Gateway p.Path)
        //     |> fst                                                    
    }
    
// let nextEdge graph agentSmith gateway =         
//     let path = shortestPath agentSmith gateway graph
//     Option.map (fun p -> computeAllPathsWithEdgeExtracted graph p agentSmith gateway
//                          |> Seq.maxBy (fun (e, path) -> Path.totalLength gateway p)
//                          |> fst)


