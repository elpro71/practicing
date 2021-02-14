//module Test.GraphTools
namespace Test

open Xunit
open Shared
open GraphTools

open FsCheck
open FsCheck.Xunit
open GraphModel
open GraphAdapters
open Swensen.Unquote
open Xunit.Abstractions

type GraphTools(output : ITestOutputHelper) =

    [<Fact>]  
    let ``logtest`` () =
    
        let g = [ (0, 1)
                  (0, 2)
                  (1, 3)
                  (2, 3)
                  (3, 5)
                  (3, 6)
                  (5, 6) ] |> List.map Edge |> DirtyG |> G.create

        let result = shortestPath 0 3 g
        let txt = sprintf "%A" result
        output.WriteLine txt

        test <@ false @>


