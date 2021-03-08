open System
let token = (Console.In.ReadLine()).Split [| ' ' |]
let W = int (token.[0])
let H = int (token.[1])

let N =
    int (Console.In.ReadLine()) (* maximum number of turns before game over. *)

let token1 = (Console.In.ReadLine()).Split [| ' ' |]
let X0 = int (token1.[0])
let Y0 = int (token1.[1])

type TemparatureReading =
    | Colder
    | Warmer
    | Same

type Window = int * int
type FoundState = { Position: int }

type SearchState =
    | InitState of int * Window
    | RunningState of int * int * Window
    | ResyncColderState of int * int * Window
    | ResyncColderAtTwoThirdState of int * int * Window
    | FoundState of int

module SearchState =
    let getPosition =
        function
        | InitState (x, _) -> x
        | RunningState (_, x, _) -> x
        | ResyncColderState (_, x, _) -> x
        | ResyncColderAtTwoThirdState (_, x, _) -> x
        | FoundState x -> x

    let printDebug = ()

let getInitRead (p: int) w =
    let (w1, w2) = w

    if Math.Abs(p - w1) > Math.Abs(p - w2) then
        (p + w1) / 2
    else
        (p + w2) / 2

let readTemperature _ =
    match Console.In.ReadLine() with
    | "WARMER" -> Some Warmer
    | "COLDER" -> Some Colder
    | "SAME" -> Some Same
    | "UNKNOWN" -> None
    | _ -> failwith "invalid input"

let mutable stateOnX = InitState(X0, (0, W))

let mutable stateOnY =
    RunningState(Y0, (getInitRead Y0 (0, H)), (0, H))

let oneThird (w1, w2) = (w1 + w1 + w2) / 3
let twoThird (w1, w2) = (w1 + w2 + w2) / 3

let solve startState state (temp: TemparatureReading option) =
    eprintfn "Temp %A; State %A" temp state
    match temp with    
    | None -> startState
    | Some Warmer ->
        match state with
        | InitState (p, w) ->
            eprintfn "state machine :running from init state should have returned UNKNOWN"
            failwith ""
        | RunningState (prev, pos, (w1, w2)) ->
            let b1 = (pos + prev) / 2
            let (nw1, nw2) = 
                if prev < pos then
                    b1, Math.Max(w1,w2)
                else
                    Math.Min(w1,w2), b1+1

            let np = (pos + nw2) / 2
            RunningState(pos, np, (nw1, nw2))
        | ResyncColderState (_prev, oneThird, w) ->
            let twoThird = twoThird (w)
            ResyncColderAtTwoThirdState(oneThird, twoThird, w)
        | ResyncColderAtTwoThirdState (oneThird, twoThird, (w1, w2)) ->
            let atEquiDist = (oneThird + twoThird) / 2

            let (nb1, nb2) =
                if oneThird < twoThird then
                    (atEquiDist, Math.Max(w1, w2))
                else
                    (Math.Min(w1, w2), atEquiDist + 1)
            let median = (nb1 + nb2) / 2
            RunningState(twoThird, median, (nb1, nb2))
        | _ ->
            eprintfn "received Warmer message from unexpected state %A" state
            failwith ""
    | Some Colder ->
        match state with
        | InitState (p, w) ->
            eprintfn "state machine :running from init state should have returned UNKNOWN"
            failwith ""
        | RunningState (prev, pos, (w1, w2)) ->
            let atEquiDist = (prev + pos) / 2
            let nw =
                if prev <= pos then
                    Math.Min(w1, w2), atEquiDist+1
                else
                    atEquiDist, Math.Max(w1, w2)

            let oneThird = oneThird (nw)
            ResyncColderState(pos, oneThird, nw)
        | ResyncColderState _ ->
            eprintfn "unexpected received Colder on state %A" state
            failwith ""
        | ResyncColderAtTwoThirdState (oneThird, twoThird, (w1, w2)) ->
            let atEquiDist = (oneThird + twoThird) / 2
            let (nb1, nb2) = (Math.Min(w1, w2)), atEquiDist+1
            let median = (nb1 + nb2) / 2
            RunningState(twoThird, median, (nb1, nb2))
        | FoundState _ ->
            eprintfn "too far"
            failwith ""
    | Some Same ->
        match state with
        | InitState _ ->
            eprintfn "Unexpected same from first reading"
            failwith ""
        | RunningState (prev, pos, _) -> (prev + pos) / 2 |> FoundState
        | ResyncColderState (prev, pos, _) -> (prev + pos) / 2 |> FoundState
        | ResyncColderAtTwoThirdState (prev, pos, _) -> (prev + pos) / 2 |> FoundState
        | _ ->
            eprintfn "get same from unexpectected tate %A" state
            failwith ""

let flip f a b = f b a

let startH =
    let np = getInitRead X0 (0, W)
    RunningState(X0, np, (0, W))

let startV =
    let np = getInitRead Y0 (0, H)
    RunningState(Y0, np, (0, H))

let _ =
    Seq.initInfinite readTemperature
    |> Seq.map (fun x -> solve startH stateOnX x)
    |> Seq.map
        (fun newState ->
            stateOnX <- newState
            SearchState.getPosition stateOnX
            |> (fun x -> printfn "%d %d" x Y0)

            stateOnX)
    |> Seq.takeWhile
        (function
        | FoundState x ->
            stateOnX <- FoundState x
            false
        | _ -> true)
    |> Seq.last
    
let found = 
    match stateOnX with
    | FoundState x -> 
        eprintfn "---------------"
        eprintfn "--System identifies %d as X " x
        eprintfn "---------------"
        x
    | _ -> failwith ""

let _ =
    Seq.append [ None ] (Seq.initInfinite readTemperature)
    |> Seq.map (fun x -> solve startV stateOnY x)
    |> Seq.map
        (fun newState ->
            stateOnY <- newState
            SearchState.getPosition stateOnY
            |> (fun y -> printfn "%d %d" found y)

            stateOnY)
    |> Seq.takeWhile
        (function
        | FoundState x ->
            stateOnY <- FoundState x            
            false
        | _ -> true)
    |> Seq.last

match stateOnY with
| FoundState y -> printfn "%d %d" found y
| _ -> eprintfn "error could not find a solution"
