module Test.Graph

open Shared
open FsCheck
open FsCheck.Xunit
open GraphModel
open GraphAdapters
open Swensen.Unquote

type GraphGenerators =
  static member G() =
      {   new Arbitrary<G>() with
            override x.Generator = 
              Gen.choose(0,100) 
              |> Gen.two 
              |> Gen.map Edge.create 
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



[<Property(Arbitrary = [|typeof<GraphGenerators> |])>]
let ``test remove edges from a graph gives right remaing graph`` graph =
  let (G edges) = graph
  let medianIndex = List.length  edges / 2
  let median = List.item medianIndex edges
  let x = Edge.unwrap median 
  let flipped = tupleFlip x |> Edge.create
  let (G revisited) = G.without graph median

  test <@ 
          List.length revisited + 2 = List.length edges
                                  &&
          List.tryFind (flip List.contains [median;flipped]) revisited = None @>

