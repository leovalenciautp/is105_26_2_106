module App.Enrutador

open App.Tipos

type EstadosEnrutador =
| MostrarMenu
| MostrarJuego
| Terminar

let estadoInicial = MostrarMenu

let rec loopPrincipal estado =
    let nuevoEstado =
        match estado with 
        | MostrarMenu ->
            match Menu.mostrar() with 
            | NuevoJuego -> MostrarJuego
            | CargarJuego -> Terminar
            | Salir -> Terminar
        | MostrarJuego ->
            Juego.mostrar() 
            MostrarMenu
        
        | Terminar ->
            Terminar
    if nuevoEstado <> Terminar then 
        loopPrincipal nuevoEstado

let mostrar() =
    estadoInicial
    |> loopPrincipal
