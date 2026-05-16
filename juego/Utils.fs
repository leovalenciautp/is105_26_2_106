//
// Programa con ejemplo de animaciones para ser usado como base
// del trabajo final de la materia de Programacion I.
//
// El codigo es libre y puede ser modificado.
// Autor: Leonardo Valencia Olarte
// leonardo.valencia@utp.edu.co
//

module App.Utils

open System


//
// Funcion simple para mostrar un mensaje en la pantalla
// en las coordenadas (x,y) y con el color especififado.
//
let mostrarMensaje x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

    