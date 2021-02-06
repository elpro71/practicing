open System


type Env = 
    | TestScenario0
    | TestScenario1 
    | Prod


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
    module NodeAdjacency = let unwrap (NodeAdjacency x) = x

    type Graph = private Graph of NodeAdjacency list
    let EmptyGraph size = NodeAdjacency [] |> konst |> List.init size |> Graph

    // let addEdge (Graph graph) n1 n2 = 
    //     let add (NodeAdjacency adjList) n = n :: adjList |> NodeAdjacency 
    //     add graph.[n1] n2
            
    let read (edgeCountReader:Reader<Env,int>) (edgeReader:Reader<Env, (int * int)>) =
        let readEdges nbrLines = 
            [ 1.. nbrLines]
            |> List.map (konst edgeReader)
            |> Reader.sequence
        Reader.bind readEdges edgeCountReader       
        |> Reader.map (List.map Edge >> G)


    let (|AsAdjGraph|) (G edges) =
    //let create (G edges) =
        let getMax (Edge (x, y)) = Math.Max(x,y)
        let lastIndex = edges |> List.map getMax |> List.max
        let update (Graph lst) edges =
            let updateAdjacency (edges :Edge list) index (NodeAdjacency lst) =
                let compEdge g index (Edge e) = g e = index               
                let fromEdges = edges |> List.filter (compEdge fst index) |> List.map (Edge.unwrap >> snd)                
                let toEdges = edges |> List.filter (compEdge snd index) |> List.map (Edge.unwrap >> fst)                
                lst @ fromEdges @ toEdges|> List.distinct |> NodeAdjacency 

            lst
            |> List.mapi (updateAdjacency edges)
            |> Graph
        update (EmptyGraph (lastIndex+1)) edges
       
    let (|AsAdjList|) (Graph lst) =
        lst 
        |> List.mapi (fun i (NodeAdjacency l) -> List.map (fun x -> Edge (i, x)) l)
        |> List.concat
        |> G

    type PathResult = private PathResult of (int*int* int list)
    module PathResult =
        let createRoot n = PathResult (n, 0, [])
        let createChild (PathResult (p, n, _)) c = PathResult (c, n+1, [p])
        let addParent (PathResult (parent, _, _)) (PathResult (c, l, parents)) =  PathResult(c, l, parents @ [parent])
        let sameNode (PathResult (p, _, _)) (PathResult (q, _, _)) = p = q
        let node (PathResult (p, _, _)) = p
        let distance (PathResult (_, distance, _)) = distance
        let parents (PathResult (_, _, parents)) = parents
            

    type Path = private Path of PathResult list
    module Path =
        let create root = [ PathResult.createRoot root ] |> Path
        // add element to path        
        let skip s (Path p) = List.skip s p |> Path

    let merge (Path path) (children:PathResult list) : Path =
        let reduce (children: PathResult list) k = 
            let search = List.tryFindIndex (fun x -> PathResult.node x = PathResult.node k) children
            match search with
            | Some index when (PathResult.distance k) = (PathResult.distance children.[index]) ->
                PathResult.addParent k children.[index]
            | _ -> k
        let reducedPath = path |> List.map (reduce children) 
        let missingElements = 
            children 
            |> List.filter (fun c -> List.exists(fun r -> PathResult.node r = PathResult.node c)  reducedPath |> not)
        reducedPath @ missingElements |> Path

    let bf (Graph lst) (p:int) =
        let rec bF buf beginOfq =
            seq {
                let sbuf = Path.skip beginOfq buf
                match sbuf with
                | Path (c::_) -> 
                    yield c
                    let (NodeAdjacency children) = lst.[PathResult.node c]
                    let cpath = children |> List.map (PathResult.createChild c)
                    yield! bF (merge buf cpath) (beginOfq+1)
                | _ -> yield! [] }
        bF (Path.create p) 0

[<AutoOpen>]
module Acquisition = 
    let readNbrEdge = 
        function 
            | TestScenario0 -> 10
            | TestScenario1 -> 4
            | Prod -> Int32.Parse(Console.In.ReadLine())
        |> Reader

    let graphs = [
             [ (0,1); (0,2);(1,3); (2,4); (2,5); (3,6); (4,7); (5, 7); (6,8); (7, 8) ]
             [ (0, 1); (0, 2); (1, 2); (1, 3) ]    
    ]


    let readEdge =
        let mutable currentLine = 0

        fun env ->
            match env with
            | TestScenario0 -> 
                let index = currentLine         
                currentLine <- currentLine + 1
                graphs.[0].[index]

            | TestScenario1 -> 
                let index = currentLine         
                currentLine <- currentLine + 1
                graphs.[1].[index]

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
        Model.read readNbrEdge readEdge
        |> Reader.run TestScenario0     

    let asGraph = function | AsAdjGraph g -> g
    let asAdjList = function | AsAdjList g -> g

    let myGraph = asGraph gr

    let nicePrint pr =
        printfn "Node %i" (PathResult.node pr)
        printfn "\tDistance %i" (PathResult.distance pr)
        (PathResult.parents pr)
        |> List.iter (printfn "\tfrom %i")

    bf myGraph 0 
    |> Seq.toList
    |> Seq.iter nicePrint


    0 // return an integer exit code