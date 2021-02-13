module Test.Shared

open Xunit
open Shared

[<Fact>]
let ``queue on empty queue`` () = 
    let q = QueueWithLast<int>()

    q.Enqueue x



