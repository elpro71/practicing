module Test.Shared

open Xunit
open Shared
open Swensen.Unquote

[<Fact>]
let ``queue on empty queue`` () = 
    let q = QueueWithLast.CreateEmpty<int>()
    q.Enqueue 10

[<Fact>]
let ``queue on none empty queue`` () = 
    let q = QueueWithLast.Create([0; 2; 4])
    q.Enqueue 10

[<Fact>]
let ``dequeue on empty queue`` () = 
    let q = QueueWithLast.CreateEmpty<int>()
    test <@ q.Dequeue() = None @>

[<Fact>]
let ``dequeue on none empty queue`` () = 
    let q = QueueWithLast.Create([0; 2; 4])
    test <@ 
             [1..3] |> List.map (fun _ -> q.Dequeue()) |> List.forall Option.isSome 
             //&& q.Dequeue() = None 
     @>

[<Fact>]
let ``dequeue a singleton queue and read last element`` () = 
    let q = QueueWithLast.Create([0])
    q.Dequeue() |> ignore
    test <@ q.GetLast() =  None @>

[<Fact>]
let ``dequeue an empty queue and read last element`` () = 
    let q = QueueWithLast<int>.CreateEmpty()
    q.Dequeue() |> ignore
    test <@ q.GetLast() =  None @>

[<Fact>]
let ``dequeue a queue with more than one element and read last element`` () = 
    let lastElement = 10232
    let q = QueueWithLast.Create([ 0; 2; 4; lastElement])
    q.Dequeue() |> ignore
    test <@ q.GetLast() = Some lastElement @>

[<Fact>]
let ``compute difference betweem old an new lists`` () = 
    let a = [ 1; 2; 3; 4; ]
    let b = [ 3; 2; 5; 6 ]    
    let  (added, updated, removed) = diffBy id a b
    
    Assert.Equal<int seq>(seq { 5; 6}, added)
    Assert.Equal<int seq>(seq { 2; 3}, updated)
    Assert.Equal<int seq>(seq { 1; 4}, removed)
    // test <@
    //         let  (added, updated, removed) = diffBy id a b
    //         added = seq { 5; 6 } && updated = seq { 2; 3 } && removed =  seq { 1; 4 } 
    // @>

