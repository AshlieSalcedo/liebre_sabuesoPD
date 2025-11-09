package Heuristicas

object FuncionesGeneralesHeuristicas {

  def distanciaEnteAObjetivo(posicionEnte: Entes.Posicion, posicionObjetivo: Entes.Posicion): Int =

    Math.abs(posicionEnte.getX - posicionObjetivo.getX) + Math.abs(posicionEnte.getY - posicionObjetivo.getY)


}
