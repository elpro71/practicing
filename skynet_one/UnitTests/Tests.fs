module Test.Graph



open System
open Shared

open Xunit
open FsCheck
open FsCheck.Xunit
open GraphModel
open GraphAdapters
open Acquisition
open Swensen.Unquote


type GraphGenerators =
  static member G() =
      {   new Arbitrary<G>() with
            override x.Generator = 
              Gen.choose(0,100) 
              |> Gen.two 
              |> Gen.map Edge 
              |> Gen.sample 50 1000
              |> Gen.constant
              |> Gen.map DirtyG
              |> Gen.map G.create
      }

[<Property(Arbitrary = [|typeof<GraphGenerators> |])>]
let ``test cyclic transformation is noop`` expectedEdges =
   let graph = asGraph expectedEdges
   let computedEdges = asAdjList graph
  
   computedEdges = expectedEdges

