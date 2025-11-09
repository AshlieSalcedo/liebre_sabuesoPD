import Entes.Jugador.{Liebre, Sabuesos}
import Entorno.{Estado, TableroClasicoLyS}

@main
def main(): Unit = {

  /*

  INVOCACION BUCLE DE JUEGO SIN HEURISTICAS

  TableroClasicoLyS.bucleJuegoBasico(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))

  */

  /*

  INVOCACION BUCLE DE JUEGO CON HEURISTICA DE LIEBRE

  TableroClasicoLyS.bucleJuegoConHeuristicaLiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))

   */

  /*
  
  INVOCACION BUCLE DE JUEGO CON IA LIEBRE
  
  TableroClasicoLyS.bucleJuegoConIALiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()), true)
  
  */

  TableroClasicoLyS.bucleJuegoConIALiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()), Set(Sabuesos, Liebre))


}
