module Shared

open System.Collections.Generic

let konst x = (fun _ -> x)
let (<|) f a = a |> f
let flip f a b = f b a
let tupleToList (a, b) = [ a; b]
let tupleFlip (a, b) = b, a
let oneOfIs (a, b) x = x = a || x = b

let curry f a b = f(a,b)
let uncurry f (a,b) = f a b


// assuming we can sort
let diffBy selector olds news = 
    let doesExist n x = 
        let comp x y = selector x = selector y    
        Seq.exists (comp x) n
    
    let updated = Seq.filter (doesExist news) olds 
    let known = Seq.filter (doesExist olds >> not) news 
    let added = Seq.filter (doesExist news >> not) olds
    (known, updated, added)

type Reader<'environment,'a> = Reader of ('environment -> 'a)

type QueueWithLast<'a> =
    private
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
        if  this.Queue.Count = 0 then 
            None
        else
            let x = this.Queue.Dequeue() |> Some
            if this.Queue.Count = 0 then 
                this.Last <- None
            x
    member this.GetLast() = this.Last

    static member Create (ele: 'a seq) =
        { Queue = Queue(ele) ; Last = Seq.tryLast ele }

    static member CreateEmpty () =
        { Queue = Queue() ; Last = None}
                        
    

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

    // TODO add computation expression builder 