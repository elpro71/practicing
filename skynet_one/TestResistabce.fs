module Resistance
open System
open System.Collections.Generic


module Resistance =


    let inSerie = List.fold (+) 0.0
    let inParrallel l = 
        l 
        |> List.map (fun i -> 1.0/i) 
        |> List.fold (+) 0.0
        |> (fun s -> 1.0/s)

    let compute (resistances:IDictionary<string,float>) (tokens: string array) =
        let rec read i : (int*float) = 
            
            let rs eos (reader:int -> (int*float)) index =
                List.unfold (fun idx -> 
                    if tokens.[idx] = eos then None
                    else
                        let read = reader idx
                        Some (read, fst read)) index

            let readList endOfBlock idx = 
                let data = (rs endOfBlock read idx) 
                List.last data |> fst |> ((+) 1), data |> List.map snd

            match tokens.[i] with
            | "[" -> 
                let (nIdx, data) = readList "]" (i+1)
                nIdx, inParrallel data
            | "(" -> 
                let (nIdx, data) = readList ")" (i+1)
                nIdx, inSerie data
            | r -> 
                i+1, resistances.[r] 
           
        read 0 |> snd


let resistances = dict [ ("A", 24.0) ; ("B", 8.0); ("C", 48.0) ]
let tokens = "[ ( A B ) [ C A ] ]".Split(' ')
printfn "%A" (Resistance.compute resistances tokens)


let r0s = dict [ ("C", 20.0) ; ("D", 25.0) ]
let t0s = "[ C D ]".Split(' ')
printfn "%A" (Resistance.compute r0s t0s)


let r1s = dict [ ("Alfa", 1.0) ; ("Bravo", 1.0) ; ("Charlie",12.0); ("Delta", 4.0); ("Echo", 2.0); ("Foxtrot", 10.0); ("Golf", 8.0)]
let t1s = "( Alfa [ Charlie Delta ( Bravo [ Echo ( Foxtrot Golf ) ] ) ] )".Split(' ')
printfn "%A" (Resistance.compute r1s t1s)

let r3s = dict [ ("Star", 78.0) ]
let t3s = "[ ( [ Star ( Star Star ) ] [ Star ( Star Star ) ] Star ) ( [ Star ( Star Star ) ] [ Star ( Star Star ) ] Star ) ]".Split(' ')
printfn "%A" (Resistance.compute r3s t3s)

