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
