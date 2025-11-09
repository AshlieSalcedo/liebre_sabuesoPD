import Entorno.{Estado, TableroClasicoLyS}

@main
def main(): Unit = {

  /*

  INVOCACION BUCLE DE JUEGO SIN HEURISTICAS

  TableroClasicoLyS.bucleJuego(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))

  */

  /*

  INVOCACION BUCLE DE JUEGO CON HEURISTICA DE LIEBRE

  TableroClasicoLyS.bucleJuegoConHeuristicaLiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))

   */

  TableroClasicoLyS.bucleJuegoConIALiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()), true)


}
