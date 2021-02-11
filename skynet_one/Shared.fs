module Shared

let konst x = (fun _ -> x)
let (<|) f a = a |> f
let flip f a b = f b a

type Reader<'environment,'a> = Reader of ('environment -> 'a)

type Queue<'a> = 
    { List : list<'a>
      Last  : 'a option }

module Queue =
    let enqueue a { List = l ; Last = optiona } =
        { List = optiona |> Option.map (List.singleton >> ((@) l))  |> Option.defaultValue l
          Last = Some a }
    let dequeue =
        function
        | { List = [] ; Last = None } -> None
        | { List = [] ; Last = Some x } -> Some (x, { List = [] ; Last = None })
        | { List = h::tail; Last = last } -> Some (h, { List = tail; Last = last })
    

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
