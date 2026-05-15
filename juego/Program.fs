open System
open System.Threading

open App.Utils

type ProgramState =
| Running
| Terminated

type State = {
    ProgramState: ProgramState
    AlienX: int
    AlienY: int
    RedibujarPantalla: bool
    Tick: int
    MisilVisible: bool
    MisilX: int
    MisilY: int
}

let estadoInicial = {
    ProgramState = Running
    AlienX = Console.BufferWidth/2
    AlienY = Console.BufferHeight/2
    RedibujarPantalla = true
    Tick = -1
    MisilVisible = false
    MisilX = 0
    MisilY = 0
}

let actualizarTick state =
    {state with Tick = state.Tick+1}


let actualizarMisil state =
    if state.MisilVisible then 
        let newX = state.MisilX+1
        if newX < (Console.BufferWidth-2) then 
            {state with MisilX = newX}
        else
            {state with MisilVisible = false}

    else
        state
    |> fun newState ->
        if newState <> state then 
            {newState with RedibujarPantalla = true}
        else 
            state

let procesearTecladoApp key state =
    match key with 
    | ConsoleKey.Escape ->
        {state with ProgramState = Terminated}
    | _ -> state
let procesarTecladoDeAlien key state =
    match key with 
    | ConsoleKey.UpArrow ->
        {state with AlienY = max 0 (state.AlienY-1)}
    | ConsoleKey.DownArrow ->
        {state with AlienY = min (Console.BufferHeight-1) (state.AlienY+1)}

    | ConsoleKey.LeftArrow ->
        {state with AlienX = max 0 (state.AlienX-1)}
    | ConsoleKey.RightArrow ->
        {state with AlienX = min (Console.BufferWidth-2) (state.AlienX+1)}

    | ConsoleKey.Spacebar ->
        if not state.MisilVisible then 
            {state with MisilVisible = true; MisilY=state.AlienY; MisilX = state.AlienX+2 }
        else
            state
    | _ ->
        state
    |> fun newState ->
        if newState <> state then 
            {newState with RedibujarPantalla=true}
        else
            state

let procesarTeclado state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state 
        |> procesearTecladoApp k.Key
        |> procesarTecladoDeAlien k.Key
    else
        state

let redibujarAlien state =
    mostrarMensaje state.AlienX state.AlienY ConsoleColor.Yellow "👽"

let redibujarMisil state =
    if state.MisilVisible then 
        mostrarMensaje state.MisilX state.MisilY ConsoleColor.Yellow "=>"

let redibujarPantalla state =
    if state.RedibujarPantalla then 
        Console.Clear()
        [|
            redibujarAlien
            redibujarMisil
        |] |> Array.iter ( fun f -> state |> f)
        {state with RedibujarPantalla=false}
    else
        state


let rec mainLoop state =
    let newState =
        state 
        |> actualizarTick
        |> actualizarMisil
        |> procesarTeclado
        |> redibujarPantalla
    if newState.ProgramState <> Terminated then 
        Thread.Sleep 25
        newState |> mainLoop

Console.Clear()
Console.CursorVisible <- false

estadoInicial
|> mainLoop

Console.CursorVisible <- true
Console.Clear()