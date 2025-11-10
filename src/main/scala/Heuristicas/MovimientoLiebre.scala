package Heuristicas

import Entorno.{Estado, TableroJuego}

import scala.annotation.tailrec

case object MovimientoLiebre {


  extension [X, Y](t: (X, Y))

    def toStringBonitoLiebre: String =

      "(" + t._1 + ", " + t._2 + ")"


  @tailrec
  //Devuelve la distancia de una posición con los sabuesos
  private def distanciaASabuesos(movimientoAEvaluar: Entes.Posicion, tablero: TableroJuego, estado: Estado, sabuesosAEvaluar: Set[Entes.Posicion], distanciaAcumulada: Int = 0): Int =

    if (sabuesosAEvaluar.nonEmpty)

      val sabuesoSiendoEvaluado = sabuesosAEvaluar.head
      distanciaASabuesos(movimientoAEvaluar, tablero, estado, sabuesosAEvaluar - sabuesoSiendoEvaluado, distanciaAcumulada + FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(movimientoAEvaluar, sabuesoSiendoEvaluado))

    else

      distanciaAcumulada

//Si uno o más de un sabueso ha sido rebasado entonces será verdadero
  def algunSabuesoHaSidoRebasado(tablero: TableroJuego, estado: Estado): Boolean =

    FuncionesGeneralesHeuristicas.sabuesosRebasadosPorMovimiento(estado.Liebre, tablero, estado) > 0


  def evaluarMovimiento(tablero: TableroJuego, estado: Estado, destino: Entes.Posicion): (Int, Int) = {

    //Si algún sabueso ha sido rebasado devolvemos la distancia de la nueva posición y la meta y la distancia de la nueva posición y los sabuesos
    if (algunSabuesoHaSidoRebasado(tablero, estado)) {

      (FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(destino, tablero.posicionMetaLiebre), distanciaASabuesos(destino, tablero, estado, estado.Sabuesos))
    //Si no entonces devolvemos el número de sabuesos que se rebasaran con ese movimiento y la distancia de la nueva posición y los sabuesos
    } else

      (FuncionesGeneralesHeuristicas.sabuesosRebasadosPorMovimiento(destino, tablero, estado), distanciaASabuesos(destino, tablero, estado, estado.Sabuesos))
  }

  

}