import Entorno.{Estado, TableroClasicoLyS}

@main
def main(): Unit = {

  TableroClasicoLyS.bucleJuego(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))

}
