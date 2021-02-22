module X


#if INTERACTIVE
#load "Shared.fs"
#load "Model.fs"
#load "GraphTools.fs"
#load "Acquisition.fs"
#load "GraphStructureAdapters.fs"
#endif 

open System
open Shared
open GraphModel
open GraphTools
open Acquisition
open GraphAdapters
open PuzzleLogic


let init () =
    let gr =
        read readNbrEdge readEdge
        |> Reader.run TestScenario2     
        |> G.create

    asGraph gr

let nicePrint pr =
    printfn "Node %i" pr.Node
    printfn "\tDistance %i" pr.DistanceFromOrigin
    pr.Parents
    |> List.iter (printfn "\tfrom %i")

// best next move based on one computation iteration.


let printSize = G.unwrap >> List.length >> printfn "%d"

type S = { Idx : int; Count : int; TotCount : int }
module S = 
    let addZero prevIndex seq =
        if seq.Idx = -1 then        
            { Idx = prevIndex + 1; Count = 0; TotCount = seq.TotCount + 1 }
        else 
            { Idx = prevIndex + 1 ; Count = 0; TotCount = seq.Count + 1 }
                    
    let addOne sequence = { sequence with Count = sequence.Count + 1 ; TotCount = sequence.TotCount + 1 }
    let max s ns = if ns.TotCount > s.TotCount then ns else s

type State = { Idx: int ; Cur : S ; Max : S }
module State = 
    let initSeq = { Idx = -1 ; Count = 0 ; TotCount = 0 }
    let empty = { Idx = 0; Cur = initSeq; Max = initSeq }

    let addZero index current max = 
        let nIdx = index + 1
        let n = S.addZero index current
        { Idx = nIdx ; Cur = n ; Max = S.max max n }

    let addOne index current max  = 
        let c = S.addOne current        
        { Idx = index + 1 ; Cur = c ; Max = S.max c max }  

    let folder { Idx = prevIndex; Cur= current ; Max = max }  char = 
        let idx = prevIndex
        if char = '1' then 
            addOne idx current max
        else 
            addZero idx current max
       

let solve (string :String)= 
    string.ToCharArray() 
    |> (Array.fold State.folder State.empty)
    |> (fun solution -> printfn "%d" solution.Max.TotCount)



// let distance (ar1 : int array) (ar2 : int array) = abs (ar1.[0]-ar2.[0]) + abs (ar1.[1]-ar2.[1])
// let getdata () = 
//     [ 1 .. (int(Console.In.ReadLine())) ]
//     |> List.map (fun _ -> 
//         Console.In.ReadLine().Split(' ')
//         |> Array.map int)
// let data = getdata ()
// let getMin d i e = 
//     d
//     |> List.mapi (curry id)
//     |> List.filter (fst >> ((<>) i))
//     |> List.map (snd >> distance e)
//     |> List.min
// data
// |> List.mapi (fun index e -> getMin data index e)
// |> List.min
// |> (printfn "%d")
        

[<EntryPoint>]
let main argv =


    // let resistances = dict [ ("Star", 78.0) ]
    // let tokens = "[ ( [ Star ( Star Star ) ] [ Star ( Star Star ) ] Star ) ( [ Star ( Star Star ) ] [ Star ( Star Star ) ] Star ) ]".Split(" ")                
    // printfn $"Circuit resistance equals to {Resistance.Resistance.compute resistances tokens}" 

     
    // let x  =  "1011".ToCharArray() |> Array.fold folder initialState 
    // let x1 =  "00".ToCharArray() |> Array.fold folder initialState 
    // let x2 =  "1".ToCharArray() |> Array.fold folder initialState 
    // let x3 =  "11".ToCharArray() |> Array.fold folder initialState 
    //let x4 =  "01101".ToCharArray() |> Array.fold folder initialState 
    //let x =  "011001".ToCharArray() |> Array.fold folder initialState 
             //0123456789012
    
    // let x4 =  "011011101111011111000000".ToCharArray() |> Array.fold State.folder initialState 
    // while true do 
    //     let solution = Console.In.ReadLine().ToCharArray() |> Array.fold State.folder initialState
    //     printfn "by flipping at %d then a max seq of lenght %d is found" solution.Max.Idx solution.Max.TotCount

    //let solution =  "000".ToCharArray() |> Array.fold folder initialState 
    //printfn "by flipping at %d then a max seq of lenght %d is found" solution.Max.Idx solution.Max.TotCount

    //let mutable g = 
        // [   (6, 11)
        //     (0, 9)
        //     (1, 2)
        //     (0, 1)
        //     (1, 10)
        //     (5, 11)
        //     (2, 3)
        //     (4, 5)
        //     (8, 9)
        //     (6, 7)
        //     (7, 8)
        //     (0, 6)
        //     (3, 4)
        //     (0, 2)
        //     (7, 11)
        //     (0, 8)
        //     (0, 4)
        //     (9, 10)
        //     (0, 5)
        //     (0, 7)
        //     (0, 3)
        //     (0, 10)
        //     (5, 6)
        //     (11, 6)
        //     (9, 0)
        //     (2, 1)
        //     (1, 0)
        //     (10, 1)
        //     (11, 5)
        //     (3, 2)
        //     (5, 4)
        //     (9, 8)
        //     (7, 6)
        //     (8, 7)
        //     (6, 0)
        //     (4, 3)
        //     (2, 0)
        //     (11, 7)
        //     (8, 0)
        //     (4, 0)
        //     (10, 9)
        //     (5, 0)
        //     (7, 0)
        //     (3, 0)
        //     (10, 0)
        //     (6, 5) 
        //     ]


        //    [(2, 6)
        //     (3, 7)
        //     (3, 6)
        //     (3, 5)
        //     (3, 4)
        //     (1, 7)
        //     (0, 2)
        //     (0, 1)
        //     (0, 3)
        //     (1, 3)
        //     (2, 3)
        //     (4, 7)
        //     (5, 6) ]

        //     |> List.map Edge.create |> DirtyG |> G.create

    let g = [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 3) ] |> List.map Edge.create |> DirtyG |> G.create

    let s = shortestPath 0 3 g


    // let agentSmith = 0
    // let gateway = 4
    // let gateways = [ gateway; 5 ]
    // let spath = shortestPath agentSmith gateway g
    // printfn "%A" spath

    // g <-  G.without g (Edge.create(3,4))

    // let x =
    //     match spath with 
    //     | Some path -> 
    //         let x = Path.collectAllEdgesOnPaths path gateway  |> Seq.toList
    //         x
    //     | _ -> failwith ""



    // //output.WriteLine txt


    // printfn "%A" argv

    // Console.WriteLine($"{DateTime.Now} : Building grid")
    // let grid = TestData.makeGrid (if Array.isEmpty argv then 1000000000 else int(argv.[0])) 
    // Console.WriteLine($"{DateTime.Now} : starting")
    // let grid = TestData.makeGrid 2 1000
    // Console.WriteLine($"{DateTime.Now} : constructed as edge list")
    // let tr = asGraph grid
    // Console.WriteLine($"{DateTime.Now} : transfomed as sparse adjancency matrix")
    // let e = asAdjList tr
    // Console.WriteLine($"{DateTime.Now} : constructd back to edge list")
    
    // printSize grid
    // printSize e

    
    // let grid = 
    //     [ (0, 1)
    //       (0, 2)
    //       (2, 3)
    //       (5, 2)
    //       (6, 0)
    //       (1, 5)
    //       (7, 5) ] |> List.map Edge.create |> DirtyG |> G.create

    // let test = nextWithManyGateways grid 0  [ 3; 3 ]

    // printfn "my choice : %A" test
    0


