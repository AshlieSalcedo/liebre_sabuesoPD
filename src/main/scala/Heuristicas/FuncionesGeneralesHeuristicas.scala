package Heuristicas

import Entorno.{Estado, TableroJuego}

object FuncionesGeneralesHeuristicas {

  def distanciaEnteAObjetivo(posicionEnte: Entes.Posicion, posicionObjetivo: Entes.Posicion): Int =

    Math.abs(posicionEnte.getX - posicionObjetivo.getX) + Math.abs(posicionEnte.getY - posicionObjetivo.getY)

  def sabuesosRebasadosPorMovimiento(movimientoAEvaluar: Entes.Posicion, tablero: TableroJuego, estado: Estado): Int =

    estado.Sabuesos.map(m => m.getX).count(c => c > movimientoAEvaluar.getX)

}
