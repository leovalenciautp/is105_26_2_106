module App.Enrutador

open App.Tipos

type EstadosEnrutador =
| MostrarMenu
| MostrarJuego
| MostrarSaludo
| Terminar

let estadoInicial = MostrarMenu

let rec loopPrincipal estado =
    let nuevoEstado =
        match estado with 
        | MostrarMenu ->
            match Menu.mostrar() with 
            | NuevoJuego -> MostrarJuego
            | NuevoSaludo -> MostrarSaludo
            | Salir -> Terminar
        | MostrarJuego ->
            Juego.mostrar() 
            MostrarMenu

        | MostrarSaludo ->
            Saludo.mostrar()
            MostrarMenu
        
        | Terminar ->
            Terminar
    if nuevoEstado <> Terminar then 
        loopPrincipal nuevoEstado

let mostrar() =
    estadoInicial
    |> loopPrincipal
