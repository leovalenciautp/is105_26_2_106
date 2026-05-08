module App.Saludo

open App.Utils
open System
open System.Threading

type EstadoDePrograma =
| Ejecutando
| Terminado

type EstadoDeSaludo =
| ObteniendoInformacion
| MostrandoSaludo

type Estado = {
    EstadoDePrograma: EstadoDePrograma
    Tick: int
    Clock: int
    RedibujarPantalla: bool
    EstadoDeSaludo: EstadoDeSaludo
    SaludoX: int
    SaludoY: int
    Mensaje: string
    Buffer: string
}

let estadoInicial = {
    EstadoDePrograma = Ejecutando
    Tick = -1
    Clock = 0
    RedibujarPantalla = true
    EstadoDeSaludo = ObteniendoInformacion
    SaludoX = 0
    SaludoY = 10
    Mensaje = "Entra tu nombre: "
    Buffer = ""
}

let actualizarTick estado =
    {estado with Tick = estado.Tick+1}

let actualizarReloj estado =
    if estado.Tick <> 0 && estado.Tick % 40 = 0 then 
        {estado with Clock=estado.Clock+1; RedibujarPantalla = true}
    else
        estado
let mostrarReloj estado =
    mostrarMensajeDerecha 0 ConsoleColor.Yellow $"{estado.Clock}"
    estado

let mostrarSaludo estado =
    match estado.EstadoDeSaludo with 
    | ObteniendoInformacion ->
        mostrarMensaje estado.SaludoX estado.SaludoY ConsoleColor.Yellow estado.Mensaje
        mostrarMensaje (estado.SaludoX+estado.Mensaje.Length) estado.SaludoY ConsoleColor.Yellow estado.Buffer
        mostrarMensaje (estado.SaludoX+estado.Mensaje.Length+estado.Buffer.Length) estado.SaludoY ConsoleColor.Yellow "☠️"
    | MostrandoSaludo ->
        mostrarMensaje estado.SaludoX estado.SaludoY ConsoleColor.Cyan $"Hola {estado.Buffer}"

    
let redibujarPantalla estado =
    if estado.RedibujarPantalla then
        Console.Clear()
        estado 
        |> mostrarReloj
        |> mostrarSaludo
        |> ignore
        {estado with RedibujarPantalla = false}
    else
        estado

let procesarTecladoReloj key estado =
    match key with 
    | ConsoleKey.Escape ->
        {estado with EstadoDePrograma = Terminado}
    | _ -> estado

let procesarTecladoSaludo key estado =
    match key with 
    | ConsoleKey.Enter ->
        { estado with EstadoDeSaludo = MostrandoSaludo;RedibujarPantalla=true}
    | ConsoleKey.A ->
        { estado with Buffer = estado.Buffer + "a";RedibujarPantalla=true}
    | ConsoleKey.Spacebar ->
        { estado with Buffer = estado.Buffer + " ";RedibujarPantalla=true}
    | _ -> estado
let procesarTeclado estado =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        estado 
        |> procesarTecladoReloj k.Key
        |> procesarTecladoSaludo k.Key
    else
        estado

let rec loopPrincipal estado =
    let nuevoEstado =
        estado
        |> actualizarTick
        |> actualizarReloj
        |> procesarTeclado
        |> redibujarPantalla
    if nuevoEstado.EstadoDePrograma = Ejecutando then 
        Thread.Sleep 25
        nuevoEstado |> loopPrincipal

let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false
    let oldForeground = Console.ForegroundColor

    estadoInicial
    |> loopPrincipal

    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()