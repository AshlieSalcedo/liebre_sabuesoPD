package Heuristicas

import Entorno.{Estado, TableroJuego}

object FuncionesGeneralesHeuristicas {
  //Esta función sigue el metodo de cálculo manhattan para devolvernos un entero que es la distancia entre dos posiciones dadas
  def distanciaEnteAObjetivo(posicionEnte: Entes.Posicion, posicionObjetivo: Entes.Posicion): Int =

    Math.abs(posicionEnte.getX - posicionObjetivo.getX) + Math.abs(posicionEnte.getY - posicionObjetivo.getY)
    
  //Esta función nos devuelve la cantidad de sabuesos que rebasa una posición
  def sabuesosRebasadosPorMovimiento(movimientoAEvaluar: Entes.Posicion, tablero: TableroJuego, estado: Estado): Int =
  //cuenta cada uno de los sabuesos que cumplen que están a la derecha de esa posición
    estado.Sabuesos.map(m => m.getX).count(c => c > movimientoAEvaluar.getX)

}
