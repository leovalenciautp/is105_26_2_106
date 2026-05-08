module App.Juego

open System
open System.Threading

open App.Utils

type ProgramState =
| Running
| Terminated

type State = {
    ProgramState: ProgramState
    RedrawScreen: bool
    Tick: int
    Clock: int
    MonsterX: int
    MonsterY: int
    RockX: int
    RockY: int
    StartTick: int
}

let initialState = {
    ProgramState = Running
    RedrawScreen = true
    Tick = -1
    Clock = 0
    MonsterX = Console.BufferWidth/2
    MonsterY = Console.BufferHeight/2
    RockX = 5
    RockY = 0
    StartTick=0
}





let updateTick state =
    {state with Tick = state.Tick+1}

let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then
        {state with Clock=state.Clock+1;RedrawScreen=true}
    else
        state

let displayClock state =
    mostrarMensajeDerecha 0 ConsoleColor.Green $"{state.Clock}"
    state

let displayMonster state =
    mostrarMensaje state.MonsterX state.MonsterY ConsoleColor.Red "👽"
    state

let displayRock state =
    mostrarMensaje state.RockX state.RockY ConsoleColor.Red "🪨"
    state

let updateRock state =
    let t = float (state.Tick - state.StartTick)*0.025
    let y = 0.5*9.77*t**2.0
    let pixelY = min (Console.BufferHeight-1) (int (y*300.0/float Console.BufferHeight))
    if pixelY <> state.RockY then
        {state with RockY = pixelY;RedrawScreen=true}
    else
        state
let redrawScreen state =
    if state.RedrawScreen then
        Console.Clear() 
        state
        |> displayClock
        |> displayMonster
        |> displayRock
        |> fun s -> {s with RedrawScreen=false}
    else
        state

let updateClockKeyboard key state =
    match key with 
    | ConsoleKey.Escape -> 
        {state with ProgramState = Terminated}
    | _ -> state

let updateMonsterKeyboard key state =
    match key with 
    | ConsoleKey.UpArrow -> {state with MonsterY = max 0 (state.MonsterY-1)}
    | ConsoleKey.DownArrow -> {state with MonsterY = min (Console.BufferHeight-1) (state.MonsterY+1)}
    | ConsoleKey.LeftArrow -> { state with MonsterX = max 0 (state.MonsterX-1)}
    | ConsoleKey.RightArrow -> {state with MonsterX = min (Console.BufferWidth-2) (state.MonsterX+1)}
    | _ -> state
    |> fun s ->
        if s <> state then 
            {s with RedrawScreen = true}
        else
            state

let updateRockKeyboard key state =
    match key with 
    | ConsoleKey.Enter -> {state with RockY = 0;StartTick=state.Tick; RedrawScreen= true}
    | _ -> state
let processKeyboad state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state 
        |> updateClockKeyboard k.Key
        |> updateMonsterKeyboard k.Key
        |> updateRockKeyboard k.Key
    else
        state
let rec mainLoop state =
    let newState =
        state
        |> updateTick
        |> updateClock
        |> processKeyboad
        |> updateRock
        |> redrawScreen
    if newState.ProgramState = Running then
        Thread.Sleep 25
        mainLoop newState

let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false
    let oldForeground = Console.ForegroundColor

    initialState
    |> mainLoop

    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()

