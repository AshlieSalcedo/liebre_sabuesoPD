package Heuristicas

case object MovimientoSabuesos {


  //(Boolean, Int, Int) = (Es rebasado?, Distancia a la liebre, Distancia al resto de sabuesos)
  def evaluarMovimiento(tablero: Entorno.TableroJuego, estado: Entorno.Estado, posicionSabuesoActual: Entes.Posicion, posicionAnalizandose: Entes.Posicion): (Boolean, Int, Int) =
 
    val seraRebasado = posicionAnalizandose.getX >= estado.Liebre.getX
    val distanciaALaLiebre = FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(posicionAnalizandose, estado.Liebre)
    val sabuesoAux = (estado.Sabuesos - posicionSabuesoActual).head
    val distanciaAlRestoSabuesos = FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(posicionAnalizandose, sabuesoAux) + FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(posicionAnalizandose, (estado.Sabuesos -- Set(posicionSabuesoActual, sabuesoAux)).head)


    (seraRebasado, distanciaALaLiebre, distanciaAlRestoSabuesos)

  extension [Y, Z](t: (Boolean, Y, Z))
    
    def toStringBonitoSabuesos: String =
  
      if(t._1) "(Puede ser rebasado, " + t._2 + ", " + t._3 + ")"
      else "(No será rebasado, " + t._2 + ", " + t._3 + ")"


}
