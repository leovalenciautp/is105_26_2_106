open System
open System.Threading

open App.Utils

type ProgramState =
| Running
| Terminated

type Misil = {
    X: int
    Y: int
}

type EstadoDeSprite =
| Vivo
| Muerto

type State = {
    ProgramState: ProgramState
    AlienX: int
    AlienY: int
    AlienEstado: EstadoDeSprite
    ColisionAlien: int
    RedibujarPantalla: bool
    Tick: int
    Misiles: Misil list
    EnemigoX: int
    EnemigoY: int
    EnemigoDir: int
    MisilesEnemigos: Misil list
}

let estadoInicial = {
    ProgramState = Running
    AlienX = Console.BufferWidth/2
    AlienY = Console.BufferHeight/2
    AlienEstado = Vivo
    ColisionAlien = 0
    RedibujarPantalla = true
    Tick = -1
    Misiles = []
    EnemigoX= Console.BufferWidth-2
    EnemigoY= 0
    EnemigoDir=1
    MisilesEnemigos = []
}

let actualizarTick state =
    {state with Tick = state.Tick+1}


let actualizarMisiles state =
    if state.Misiles <> [] then 
        state.Misiles
        |> Seq.map (fun misil -> {misil with X = misil.X+1})
        |> Seq.filter (fun misil -> misil.X < Console.BufferWidth-2)
        |> Seq.toList
        |> fun nuevosMisiles ->
            {state with Misiles = nuevosMisiles; RedibujarPantalla=true}
    else
        state

let actualizarMisilesEnemigos state =
    if state.MisilesEnemigos <> [] then 
        state.MisilesEnemigos
        |> Seq.map (fun misil -> {misil with X = misil.X-1})
        |> Seq.filter (fun misil -> misil.X >= 0)
        |> Seq.toList
        |> fun nuevosMisiles ->
            {state with MisilesEnemigos = nuevosMisiles; RedibujarPantalla=true}
    else
        state

let actualizarEnemigo state =
    if state.Tick % 4 = 0 then 
        let nuevoY = state.EnemigoY + state.EnemigoDir
        let nuevaDir,Y = 
            match nuevoY with 
            | y when y > Console.BufferHeight-1 -> -1,Console.BufferHeight-1
            | y when y < 0 -> 1,0
            | _ -> state.EnemigoDir,nuevoY


        {state with EnemigoY = Y; EnemigoDir=nuevaDir;RedibujarPantalla=true}
    else
        state


let dispararMisilesEnemigos state =
    if state.Tick % 10 = 0 then 
        let nuevoMisil = {
            X = state.EnemigoX-2
            Y = state.EnemigoY
        }
        {state with MisilesEnemigos = nuevoMisil :: state.MisilesEnemigos; RedibujarPantalla=true}
    else
        state


let detectarColisionAlien state =
    state.MisilesEnemigos
    |> List.filter ( fun misil -> not ( misil.Y = state.AlienY && misil.X = state.AlienX+1))
    |> fun nuevosMisiles ->
        if nuevosMisiles.Length <> state.MisilesEnemigos.Length then
            {state with 
                AlienEstado = Muerto
                MisilesEnemigos = nuevosMisiles
                RedibujarPantalla=true
                ColisionAlien = state.Tick
            }
        else
            state 

let resetAlien state =
    if state.AlienEstado = Muerto then 
        let tiempo = state.Tick-state.ColisionAlien
        if tiempo >= 120 then 
            {state with AlienEstado=Vivo;RedibujarPantalla=true}
        else
            state
    else
        state

let procesearTecladoApp key state =
    match key with 
    | ConsoleKey.Escape ->
        {state with ProgramState = Terminated}
    | _ -> state
let procesarTecladoDeAlien key state =
    if state.AlienEstado = Vivo then 
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
            let nuevoMisil = {
                X = state.AlienX+2
                Y = state.AlienY
            }
            {state with Misiles = nuevoMisil :: state.Misiles}
            
        | _ ->
            state
        |> fun newState ->
            if newState <> state then 
                {newState with RedibujarPantalla=true}
            else
                state
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
    let sprite = 
        if state.AlienEstado = Vivo then 
            "👽"
        else
            "💥"
    mostrarMensaje state.AlienX state.AlienY ConsoleColor.Yellow sprite

let redibujarMisiles state =
    
    state.Misiles
    |> List.iter (fun misil ->
        mostrarMensaje misil.X misil.Y ConsoleColor.Yellow "=>"
    )

let redibujarMisilesEnemigos state =
    
    state.MisilesEnemigos
    |> List.iter (fun misil ->
        mostrarMensaje misil.X misil.Y ConsoleColor.Red "<="
    )

let redibujarEnemigo state =
    mostrarMensaje state.EnemigoX state.EnemigoY ConsoleColor.Yellow "☠️"

let redibujarPantalla state =
    if state.RedibujarPantalla then 
        Console.Clear()
        [|
            redibujarMisiles
            redibujarAlien
            redibujarEnemigo
            redibujarMisilesEnemigos
        |] |> Array.iter ( fun f -> state |> f)
        {state with RedibujarPantalla=false}
    else
        state


let rec mainLoop state =
    let newState =
        state 
        |> actualizarTick
        |> actualizarMisiles
        |> actualizarEnemigo
        |> dispararMisilesEnemigos
        |> actualizarMisilesEnemigos
        |> detectarColisionAlien
        |> resetAlien
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