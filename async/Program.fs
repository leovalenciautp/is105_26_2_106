//
// Taller programacion Asíncrona.
//
// Mayo 12 2026
//

open System
open System.Threading


let mostrarMensaje x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let morstrarMensajeDerecha y color (msg:string) =
    let start = Console.BufferWidth-msg.Length
    mostrarMensaje start y color msg
let timerUno() =
    async {
        do! Async.Sleep 5000
        printfn "Timer 1 !!"
    }


let calculoLargoUno x = 
    async {
        do! Async.Sleep 7000
        printfn "Terminé el calculu Uno"
        return 2*x
    }

let calculoLargoDos x =
    async {
        do! Async.Sleep 6000
        printfn "Terminé el calculo dos"
        return 3*x
    }

let evaluar() = 
    async {
        let! p1 = calculoLargoUno 2 |> Async.StartChild
        let! p2 = calculoLargoDos 3 |> Async.StartChild
        let! r1 = p1
        let! r2 = p2
        let r = r1+r2
        printfn $"El resultado es {r}"
    }

//
// Hacer una funcion async recursiva que muestre
// un reloj cada segundo, algo como timer=0, timer=1, ...
//

let rec reloj x =
    async {
        morstrarMensajeDerecha 0 ConsoleColor.Yellow $"Timer={x}"
        do! Async.Sleep 1000
        return! reloj (x+1)
    }

let leerTeclado() = 
    async {
        while true do
            if Console.KeyAvailable then 
                let k = Console.ReadKey true
                mostrarMensaje 0 10 ConsoleColor.Cyan $"Tecla presionada: {k.Key}"
            //do! Async.Sleep 10
    }

Console.Clear()
Console.CursorVisible <- false

//
// Es importante que toda la Salida en pantalla del programa ocurra en el
// mismo thread. 
//
// A ese hilo se le llama el UI thread
//
leerTeclado() |> Async.Start // Arranca una computacion en el Thread pool
evaluar() |> Async.StartImmediate // Arranca una computacion en el thread actual

reloj 0 |> Async.RunSynchronously

