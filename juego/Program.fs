//
// Programa con ejemplo de animaciones para ser usado como base
// del trabajo final de la materia de Programacion I.
//
// El codigo es libre y puede ser modificado.
// Autor: Leonardo Valencia Olarte
// leonardo.valencia@utp.edu.co
//
open System
open System.Threading

open App.Utils

//
// La aplicacion tiene dos estados,
// Esta ejecutandose: Running, o esta terminando: Terminated.
//
type ProgramState =
| Running
| Terminated

//
// Los misiles por ahora, solo necesitan las coordenadas
// en pantalla
//
type Misil = {
    X: int
    Y: int
}

//
// Tanto el Alien como el Enemigo, pueden estar
// vivos o muertos.
//
type EstadoDeSprite =
| Vivo
| Muerto

//
// Este record contiene todo el estado del juego
//
type State = {
    ProgramState: ProgramState
    AlienX: int
    AlienY: int
    AlienEstado: EstadoDeSprite

    //
    // Aqui almacenamos el Tick cuando ocurre una colision con
    // el Alien.
    //
    ColisionAlien: int 

    RedibujarPantalla: bool
    Tick: int
    Misiles: Misil list
    EnemigoX: int
    EnemigoY: int
    EnemigoDir: int
    EnemigoEstado: EstadoDeSprite
    ColisionEnemigo: int
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
    EnemigoEstado = Vivo
    ColisionEnemigo = 0
    MisilesEnemigos = []
}

//
// Esta funcion cumple la mision de simular un Timer del app
// El Tick se incrementa cada 25 mili segundos.
///
let actualizarTick state =
    {state with Tick = state.Tick+1}


//
// Todos los misiles que hayamos disparado se mueven 1 a la derecha
// filtramos y solo dejamos los misiles que aun estan visibles en la
// pantalla
//
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

//
// Usamos la misma heuristica para los misiles del enemigo
// la diference es que se mueven 1 a la izquierda.
//
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

//
// El enemigo se mueve de arriba a abajo. Hay varias formas de lograr el efecto. 
// En esta funcion usamos una direccion que puede ser 1 o -1 (suma o resta)
// y la cambiamos cuando el enemigo llege a la parte inferior, o a la superior de
// la pantalla.
//
// Movemos el enemigo cada 4 ticks (100ms)
//
let actualizarEnemigo state =
    if state.EnemigoEstado = Vivo && state.Tick % 4 = 0 then 
        let nuevoY = state.EnemigoY + state.EnemigoDir
        let nuevaDir,Y = 
            match nuevoY with 
            | y when y > Console.BufferHeight-1 -> -1,Console.BufferHeight-1
            | y when y < 0 -> 1,0
            | _ -> state.EnemigoDir,nuevoY


        {state with EnemigoY = Y; EnemigoDir=nuevaDir;RedibujarPantalla=true}
    else
        state


//
// El enemigo mos dispara msiiles cada 10 Ticks (250ms).
// El nuevo misil disparado lo agregamos a la lista de misiles
//
let dispararMisilesEnemigos state =
    if state.EnemigoEstado = Vivo && state.Tick % 10 = 0 then 
        let nuevoMisil = {
            X = state.EnemigoX-2
            Y = state.EnemigoY
        }
        {state with MisilesEnemigos = nuevoMisil :: state.MisilesEnemigos; RedibujarPantalla=true}
    else
        state

//
// Aqui miramos si algun misil del enemigo ocupa la misma coordinada del Alien.
// En este caso sumamos 1 a AlienX. para que la explosion ocurra cuando el misil 
// haya tocado la mitad del emoji (recuerde que ocupan dos espacios).
//
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

//
// Heuristica muy similar para el enemigo, miramos si alguno
// de nuestros misiles lo logro golpear.
//
let detectarColisionEnemigo state =
    state.Misiles
    |> List.filter ( fun misil -> not ( misil.Y = state.EnemigoY && misil.X = state.EnemigoX-1))
    |> fun nuevosMisiles ->
        if nuevosMisiles.Length <> state.Misiles.Length then
            {state with 
                EnemigoEstado = Muerto
                Misiles = nuevosMisiles
                RedibujarPantalla=true
                ColisionEnemigo = state.Tick
            }
        else
            state 

//
// Hay que revivir al Alien despues de un tiempo, en este caso
// 120 ticks (que euivalen a 3 segundos)
//
let resetAlien state =
    if state.AlienEstado = Muerto then 
        let tiempo = state.Tick-state.ColisionAlien
        if tiempo >= 120 then 
            {state with AlienEstado=Vivo;RedibujarPantalla=true}
        else
            state
    else
        state

//
// Reusamos la misma heuristica para revivir al enemigo
//
let resetEnemigo state =
    if state.EnemigoEstado = Muerto then 
        let tiempo = state.Tick-state.ColisionEnemigo
        if tiempo >= 120 then 
            {state with EnemigoEstado=Vivo;RedibujarPantalla=true}
        else
            state
    else
        state

//
// Funciones que procesan teclas. 
// Esta funcion mira si se presionó la tecla Escape
// y termina el programa.
//
let procesearTecladoApp key state =
    match key with 
    | ConsoleKey.Escape ->
        {state with ProgramState = Terminated}
    | _ -> state

//
// El Alien responde a las flechas del teclado y a la barra
// espaciadora.
//
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
            // Esta construccion la usamos 
            // para marcar que la pantalla se debe
            // redibujar si cambio algo del estado
            //
            if newState <> state then 
                {newState with RedibujarPantalla=true}
            else
                state
    else
        state

//
// Esta es la funcion global que mira si hay teclas 
// disponibles para leer, y las lee y se las pasa
// a los otros procesadores.
//
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
        mostrarMensaje misil.X misil.Y ConsoleColor.Cyan "<="
    )

let redibujarEnemigo state =
    let sprite = 
        if state.EnemigoEstado = Vivo then 
            "☠️"
        else
            "💥"
    mostrarMensaje state.EnemigoX state.EnemigoY ConsoleColor.Yellow sprite

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
        |> detectarColisionEnemigo
        |> resetAlien
        |> resetEnemigo
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