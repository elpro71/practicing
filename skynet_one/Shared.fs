module Shared

open System.Collections.Generic

let konst x = (fun _ -> x)
let (<|) f a = a |> f
let flip f a b = f b a

type Reader<'environment,'a> = Reader of ('environment -> 'a)

type QueueWithLast<'a> = 
    { mutable Queue : Queue<'a>
      mutable Last  : 'a option }
    with 
    member this.Enqueue a : unit = 
        match this.Last with 
        | None -> this.Last <- Some a 
        | Some x -> 
            this.Queue.Enqueue x |> ignore
            this.Last <- Some a

    member this.Dequeue () : 'a option = 
        match this.Queue.Count, this.Last with 
        | 0, None  -> None
        | 0, Some x -> 
               this.Last <- None  
               Some x
        | _ -> 
            let res = this.Queue.Dequeue ()
            if this.Queue.Count = 0 then 
                this.Last |> Option.map (fun x -> this.Queue.Enqueue x) |> ignore
                this.Last <- None
            Some res

                        
    

module Reader = 

    /// Evaluate the action with a given environment
    /// 'env -> Reader<'env,'a> -> 'a
    let run environment (Reader action) = 
        let resultOfAction = action environment
        resultOfAction

    /// ('a -> 'b) -> Reader<'env,'a> -> Reader<'env,'b>
    let map f action = 
        let newAction environment =
            let x = run environment action 
            f x
        Reader newAction

    /// 'a -> Reader<'env,'a>
    let retn x = 
        let newAction environment =
            x
        Reader newAction

    /// Reader<'env,('a -> 'b)> -> Reader<'env,'a> -> Reader<'env,'b>
    let apply fAction xAction = 
        let newAction environment =
            let f = run environment fAction 
            let x = run environment xAction 
            f x
        Reader newAction

    /// ('a -> Reader<'env,'b>) -> Reader<'env,'a> -> Reader<'env,'b>
    let bind f xAction = 
        let newAction environment =
            let x = run environment xAction 
            run environment (f x)
        Reader newAction

    let sequence readers =
        let fold acc r =
            Reader (fun env ->
                                let x = run env r
                                let l = run env acc
                                x::l)
        List.fold fold  (retn []) readers
