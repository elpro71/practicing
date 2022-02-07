module Tst
open System
let size = 1000 // int(Console.In.ReadLine())
//Console.In.ReadLine() |> ignore
//let words = (Console.In.ReadLine()).Split [|' '|]
let coins = [1;2;3;4;5] // words |> Array.map int |> Array.toList
let space = size + (List.max coins)

let mask = [| for i in 0 .. space -> 0 |]
coins |> List.iter (fun c ->  mask. [ c ] <- 1 )

let next m prev = 
    m
    |> Array.skip (prev+1)
    |> Array.findIndex (fun m -> m <> 0)
    |> (fun x-> x+prev+1)
        
let mutable s = 0
while s < size  do
    s <- next mask s
    for c in coins do
        mask.[s] <- mask.[s-c]
    
printfn "%d" mask.[size]
//[ 0 .. 1000 ]
//|>  





// type Combinaison = { Set : int Set; Number : int }
// module Combinaison = 
//     let Empty = { Set = Set.empty ; Number = 1 }
//     let Add coin { Set = set ; Number = nbr } =
//         if Set.contains coin set then 
//             { Set = set ; Number = nbr }  
//         else
//             { Set = Set.add coin set ; Number = nbr }  

// type Level =   { Combinaisons : Combinaison Set }
// module Level = 
//     let Empty = { Combinaisons = Set.singleton Combinaison.Empty }
//     let add { Combinaisons = combinaisons } (coin:int) =
//         { Combinaisons = 
//             if combinaisons = Empty.Combinaisons then
//                 Set.singleton { Set = Set.singleton coin; Number = 1 }
//             else
//                 combinaisons |> Set.toSeq |> Seq.map (Combinaison.Add coin) |> Set }
   
// type Solution = 
//   { Levels : System.Collections.Generic.List<Level>
//     Coins : int list     
//     Counter : int }   
   
//     member _this.add current = 
//             let lsts =
//                 _this.Coins 
//                     |> List.choose (fun c -> 
//                         let delta = current - c 
//                         match delta with 
//                         | d when d < 0 -> None
//                         | d  -> Level.add _this.Levels.[d] c |> Some)
//             if List.isEmpty lsts |> not then 
//                 _this.Levels.[current] <- 
//                     lsts
//                     |> List.reduce (fun l1 l2 -> { Combinaisons = Set.union (Set l1.Combinaisons) (Set l2.Combinaisons ) } )

//             else
//                 ()

// module Solution = 
//     let create coins n = 
//         let initSeq = List.init n (fun i -> Level.Empty)
//         let rc = new System.Collections.Generic.List<Level>(initSeq)        
//         { Counter = 0 ; Levels = rc; Coins = coins }

//     let printSolution  level { Solution.Levels = levels } : unit =
//         levels.[level].Combinaisons |> Set.toSeq |> Seq.sumBy(fun x -> x.Number) |> printfn "%d" 
    
// let soluce = Solution.create coins (size+1)
// List.init (size+1) soluce.add |> ignore
// Solution.printSolution (size) soluce |> ignore