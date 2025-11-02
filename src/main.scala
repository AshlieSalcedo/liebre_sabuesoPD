object PruebaTablero extends App:
  // Mostramos las posiciones iniciales
  println("Posición inicial de la liebre: " + TableroClasicoLyS.posicionInicialLiebre)
  println("Posiciones iniciales de los sabuesos: " + TableroClasicoLyS.posicionesInicialesSabuesos)
  println("Meta de la liebre: " + TableroClasicoLyS.posicionMetaLiebre)

  // Probamos movimientos desde cada nodo
  val todasPosiciones = Set(
    TableroClasicoLyS.I1A, TableroClasicoLyS.MA, TableroClasicoLyS.D1A,
    TableroClasicoLyS.I2M, TableroClasicoLyS.I1M, TableroClasicoLyS.MM, TableroClasicoLyS.D1M, TableroClasicoLyS.D2M,
    TableroClasicoLyS.I1B, TableroClasicoLyS.MB, TableroClasicoLyS.D1B
  )

  todasPosiciones.foreach { pos =>
    println(s"Movimientos posibles desde $pos: ${TableroClasicoLyS.movimientosDesde(pos)}")
  }

  // Pintamos el tablero en el estado inicial
  val estadoInicial = Estado(
    Liebre = TableroClasicoLyS.posicionInicialLiebre,
    Sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
    turno = Estado.sortearTurno()
  )
  TableroClasicoLyS.pintarTablero(estadoInicial)