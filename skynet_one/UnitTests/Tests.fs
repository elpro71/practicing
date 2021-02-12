module Tests

open System
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
              gen {
                  //return TestData.getDirtyGraph 0 |> G.create
                  return TestData.makeGrid 2 
              }  }


let gen4G =   
    gen {
      Console.WriteLine("calling generator... ")
      return TestData.getDirtyGraph 0 |> G.create } 

type Marker = class end

[<Property(Arbitrary = [|typeof<GraphGenerators> |])>]
let ``test cyclic transformation is noop`` expectedEdges =
   let graph = asGraph expectedEdges
   let computedEdges = asAdjList graph
  
   computedEdges = expectedEdges

