
object TableroClasicoLyS extends TableroJuego:
  //Aqui tenemos nuestros nodos
  val I1A = Posicion(Columna.I1,Fila.A)
  val MA = Posicion(Columna.M,Fila.A)
  val D1A = Posicion(Columna.D1,Fila.A)
  val I2M = Posicion(Columna.I2, Fila.M)
  val I1M = Posicion(Columna.I1,Fila.M)
  val MM = Posicion(Columna.M,Fila.M)
  val D1M = Posicion(Columna.D1, Fila.M)
  val D2M = Posicion(Columna.D2, Fila.M)
  val I1B = Posicion(Columna.I1,Fila.B)
  val MB= Posicion(Columna.M,Fila.B)
  val D1B= Posicion(Columna.D1,Fila.B)
  //Aqui tenemos nuestras listas de adyacencia
  private val adyacencias:Map[Posicion, Set[Posicion]] = Map(
    I1A -> Set(I2M,I1M,MM,MA),
    MA -> Set(I1A,MM,D1A),
    D1A -> Set(MA,MM,D1M,D2M),
    I2M -> Set(I1A,I1M,I1B),
    I1M -> Set(I1A,I2M,I1B,MM),
    MM -> Set(MA,I1A,I1M,I1B,MB,D1B,D1M,D1A),
    D1M -> Set(D1A,MM,D1B,D2M),
    D2M -> Set(D1A,D1M,D1B),
    I1B -> Set(I2M,I1M,MM,MB),
    MB -> Set(MM,I1B,D1B),
    D1B -> Set(D2M,D1M,MM,MB)
  )
  
  def movimientosDesde(p: Posicion): Set[Posicion] = { 
    adyacencias.getOrElse(p,Set.empty) //en caso de que no sea una posición válida
  }


  // --- Posiciones iniciales de la liebre y sabuesos ---
  def posicionInicialLiebre: Posicion = D2M
    

  def posicionesInicialesSabuesos: Set[Posicion] =
    Set(I1A,I2M,I1B)

  def posicionMetaLiebre: Posicion = I2M
  
  // --- Pintado ---
  private def pintarNodo(p: Posicion, estado: Estado): String =

    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
    if (estado.Liebre == p) s"${ROJO}L${RESET}"
    else if (estado.Sabuesos.contains(p)) s"${AZUL}S${RESET}"
    else s"${BLANCO}o${RESET}"

  override def pintarTablero(estado: Estado): Unit =

    val s = pintarNodo(_, estado)
    println(s"      ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println("    ╱ |  \\  |  /  | \\")
    println(s"   ${s(I2M)}--${s(I1M)}-----${s(MM)}-----${s(D1M)}--${s(D2M)}")
    println("    \\ |  /  |   \\ | /")
    println(s"      ${s(I1B)}-----${s(MB)}-----${s(D1B)}")
  
  // --- Comprueba si ha terminado la partida ---
  def esFinPartida(estado: Estado): Option[Jugador] =
    //la liebre gana si ha llegado a la posición de meta
    if estado.Liebre == posicionMetaLiebre then
      Some(Jugador.Liebre)
    //Los sabuesos ganan si la liebre no tiene más posiciones a las que moverse
    else if MovimientoLiebre.movimientosPosibles(this,estado).isEmpty then
      Some(Jugador.Sabuesos)
    else
      None

  def bucleJuego(tablero: TableroJuego, estado:Estado):Jugador =
    //1.pintamos el tablero
    tablero.pintarTablero(estado)
    //2.Calculamos los mov posibles del jugador que tiene el turno
    val movimientos = estado.turno match
      case Jugador.Liebre => MovimientoLiebre.movimientosPosibles(tablero,estado)
      case Jugador.Sabuesos => MovimientoSabueso.movimientosPosiblePorSabueso(tablero,estado)
    //3.Mostramos por pantalla los movimientos del jugador que tiene el turno 
    movimientos.zipWithIndex.foreach{case(mov, i) => println(s"$i,$mov")}
    //4. Leer elección del jugador
    val eleccion = scala.io.StdIn.readLine().toInt
    //Se ejecuta el movimiento y se actualiza el estado
    val nuevaPos = movimientos.toSeq(eleccion)
    val nuevoEstado =
      if (estado.turno == Jugador.Liebre) 
        Estado(Liebre = nuevaPos.asInstanceOf[Posicion], Sabuesos = estado.Sabuesos, turno = Jugador.Sabuesos)
      else
        val dupla = nuevaPos.asInstanceOf[(Posicion,Posicion)]
        val origen = dupla._1
        val destino = dupla._2
        Estado(Liebre= estado.Liebre,Sabuesos = (estado.Sabuesos - origen + destino),turno = Jugador.Liebre)
    tablero.pintarTablero(nuevoEstado)    
    esFinPartida(nuevoEstado) match
      case Some(ganador) =>
        println(s"Ha ganado: $ganador!")
        ganador
      case None => bucleJuego(tablero,nuevoEstado)







     


