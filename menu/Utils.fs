module App.Utils

open System

let mostrarMensaje x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let mostrarMensajeDerecha y color (msg:string) =
    let x = Console.BufferWidth-msg.Length
    mostrarMensaje x y color msg