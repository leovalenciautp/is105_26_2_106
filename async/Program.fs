//
// Taller programacion Asíncrona.
//
// Mayo 12 2026
//

open System
open System.Threading

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

evaluar() |> Async.RunSynchronously

