package Heuristicas

import Entorno.{Estado, TableroJuego}

import scala.annotation.tailrec

case object MovimientoLiebre {


  extension [X, Y](t: (X, Y))

    def toStringBonitoLiebre: String =

      "(" + t._1 + ", " + t._2 + ")"


  @tailrec
  private def distanciaASabuesos(movimientoAEvaluar: Entes.Posicion, tablero: TableroJuego, estado: Estado, sabuesosAEvaluar: Set[Entes.Posicion], distanciaAcumulada: Int = 0): Int =

    if (sabuesosAEvaluar.nonEmpty)

      val sabuesoSiendoEvaluado = sabuesosAEvaluar.head
      distanciaASabuesos(movimientoAEvaluar, tablero, estado, sabuesosAEvaluar - sabuesoSiendoEvaluado, distanciaAcumulada + FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(movimientoAEvaluar, sabuesoSiendoEvaluado))

    else

      distanciaAcumulada


  def algunSabuesoHaSidoRebasado(tablero: TableroJuego, estado: Estado): Boolean =

    FuncionesGeneralesHeuristicas.sabuesosRebasadosPorMovimiento(estado.Liebre, tablero, estado) > 0



  def evaluarMovimiento(tablero: TableroJuego, estado: Estado, destino: Entes.Posicion): (Int, Int) = {


    if (algunSabuesoHaSidoRebasado(tablero, estado)) {

      (FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(destino, tablero.posicionMetaLiebre), distanciaASabuesos(destino, tablero, estado, estado.Sabuesos))

    } else

      (FuncionesGeneralesHeuristicas.sabuesosRebasadosPorMovimiento(destino, tablero, estado), distanciaASabuesos(destino, tablero, estado, estado.Sabuesos))
  }

  

}