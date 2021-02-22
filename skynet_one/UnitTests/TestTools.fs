//module Test.GraphTools
namespace Test

open Xunit
open FsCheck
open Swensen.Unquote
open Xunit.Abstractions

open Common
open GraphModel
open GraphTools
open PuzzleLogic

type GraphTools(output : ITestOutputHelper) =

    let g = [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 3)
              (3, 5)
              (3, 6)
              (5, 6) ] |> List.map Edge.create |> DirtyG |> G.create
    
    [<Fact>]  
    let ``test short path simple - a solution exists``() =        
        test <@ 
                optional { 
                    let! x= (shortestPath 5 1 g) 
                    return x 
                } |> Option.isSome   
        @>

    [<Fact>]  
    let ``test short path simple - a solution does not exist``() =
        test <@ 
                optional { 
                    let! x= (shortestPath 5 7 g) 
                    return x 
                } |> Option.isSome |> not
        @>

    [<Fact>]  
    let ``test short path positive -  first and last make sesnse``() =        
        
        let x = optional { 
                    let! (Path items)= (shortestPath 2 5 g) 
                    let! first = List.tryHead items
                    let! last = List.tryLast items

                    output.WriteLine($"{last}")
                    return List.contains 2 first.Parents && last.Node = 5
                }
        test <@ 
                x  = Option.Some true
        @>

    [<Fact>]  
    let ``test computation multiple exits``() =        
        test <@ PuzzleLogic.shortestPaths g 2 [ 5; 1 ] |> Seq.length = 2 @>


    [<Fact>]  
    let ``test multiple solution exits``() =        
        test <@ 
                match shortestPath  0 3 g with 
                | None -> false //failwith "expected a solution"|> Seq.length = 2
                | Some (Path p) -> (List.length p.[2].Parents) = 2
        @>




