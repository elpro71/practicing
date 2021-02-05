open System
type Env = | TestScenario1 | Prod
type Reader<'environment,'a> = Reader of ('environment -> 'a)

let konst x = (fun _ -> x)
let (<|) f a = a |> f

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

[<AutoOpen>]
module Model =
    type Edge = Edge of (int * int)
    module Edge = let unwrap (Edge c) = c

    type G = G of Edge list

    type NodeAdjacency = NodeAdjacency of int list
    type Graph = private Graph of NodeAdjacency list
    let EmptyGraph size = NodeAdjacency [] |> konst |> List.init size |> Graph

    let addEdge (Graph graph) n1 n2 = 
        let add (NodeAdjacency adjList) n = n :: adjList |> NodeAdjacency 
        add graph.[n1] n2
            
    let read (edgeCountReader:Reader<Env,int>) (edgeReader:Reader<Env, (int * int)>) =
        let readEdges nbrLines = 
            [ 1.. nbrLines]
            |> List.map (konst edgeReader)
            |> Reader.sequence
        Reader.bind readEdges edgeCountReader       
        |> Reader.map (List.map Edge >> G)

    let create (G edges) =
        let getMax (Edge (x, y)) = Math.Max(x,y)
        let lastIndex = edges |> List.map getMax |> List.max
        let update (Graph lst) edges =
            let updateAdjacency (edges :Edge list) index (NodeAdjacency lst) =
                let compEdge g index (Edge e) = g e = index
                let fromEdges = edges |> List.filter (compEdge fst index) |> List.map (Edge.unwrap >> snd)                
                lst @ fromEdges |> List.distinct |> NodeAdjacency 
            lst
            |> List.mapi (updateAdjacency edges)
            |> Graph
        update (EmptyGraph (lastIndex+1)) edges
        

module Acquisition = 
    let readNbrEdge = 
        function 
            | TestScenario1 -> 4
            | Prod -> Int32.Parse(Console.In.ReadLine())
            | _ -> failwith "what are you doing?"
        |> Reader

    let readEdge =
        let mutable currentLine = 0
        let edges = [ (0, 1); (0, 2); (1, 2); (1, 3) ]    

        fun env ->
            match env with
            | TestScenario1 -> 
                let index = currentLine         
                currentLine <- currentLine + 1
                edges.[index]

            | Prod -> 
                let values = 
                    Console.In.ReadLine().Split(' ') 
                    |> Array.map Int32.Parse
                match values with 
                | [| f ; s |] -> f,s
                | _ -> failwith " wrong ! wrong! wrong ! try again"
        |> Reader
    


// //let distance edges node1 node2 = 


[<EntryPoint>]
let main argv =

    let gr =
        Model.read Acquisition.readNbrEdge Acquisition.readEdge
        |> Reader.run Prod
     
    0 // return an integer exit code