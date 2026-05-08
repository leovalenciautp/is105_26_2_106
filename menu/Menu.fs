module App.Menu
open System
open System.Threading

//
// Esta linea trae los simbolos y funciones del
// modulo especificado
//
open App.Utils
open App.Tipos

type EstadoDePrograma = 
| Ejecutando
| Terminado



type State = {
    EstadoDePrograma: EstadoDePrograma
    RedibujarPantalla: bool
    X: int
    Y: int
    Items: (Comando * string) array
    ItemActivo: int
}

let estadoInicial = {
    EstadoDePrograma = Ejecutando
    RedibujarPantalla = true
    X = 10
    Y = 15
    Items = [|
        NuevoJuego, "New Game"
        CargarJuego, "Load Game"
        Salir, "Exit"
    |]
    ItemActivo = 0
}

let mostrarMenu estado =
    estado.Items
    |> Array.iteri ( fun i (_,etiqueta) ->
        mostrarMensaje estado.X (estado.Y+i) ConsoleColor.Yellow etiqueta 
    )

    mostrarMensaje (estado.X-2) (estado.ItemActivo+estado.Y) ConsoleColor.Cyan "☠️"

let actualizarTecladoDeMenu tecla estado =
    match tecla with 
    | ConsoleKey.Enter -> {estado with EstadoDePrograma=Terminado}
    | ConsoleKey.UpArrow -> {estado with ItemActivo = max 0 (estado.ItemActivo-1)}
    | ConsoleKey.DownArrow -> {estado with ItemActivo = min (estado.Items.Length-1) (estado.ItemActivo+1)}
    | _ -> estado
    |> fun s ->
        if s <> estado then 
            { s with RedibujarPantalla=true}
        else
            estado

let procesarTeclado estado =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        estado |> actualizarTecladoDeMenu k.Key
    else
        estado

let redibujarPantalla estado =
    if estado.RedibujarPantalla then 
        Console.Clear()
        estado |> mostrarMenu
        {estado with RedibujarPantalla=false}
    else
        estado


let rec loopPrincipal estado =
    let nuevoEstado = 
        estado
        |> procesarTeclado
        |> redibujarPantalla
    
    if nuevoEstado.EstadoDePrograma = Ejecutando then
        Thread.Sleep 25
        nuevoEstado |> loopPrincipal
    else
        nuevoEstado

let mostrar() =
    Console.CursorVisible <- false
    let colorViejo = Console.ForegroundColor

    let estado =
        estadoInicial
        |> loopPrincipal

    Console.ForegroundColor <- colorViejo
    Console.CursorVisible <- true
    Console.Clear()
    estado.Items[estado.ItemActivo] |> fst
