package Entorno

import Entes.Jugador
import Entes.Jugador.{Liebre, Sabuesos}
import Heuristicas.FuncionesGeneralesHeuristicas
import Heuristicas.MovimientoLiebre.toStringBonitoLiebre
import Heuristicas.MovimientoSabuesos.toStringBonitoSabuesos

import scala.annotation.tailrec
import scala.util.Random

object TableroClasicoLyS extends TableroJuego:

  //Aqui tenemos nuestros nodos

  private val I1A = Entes.Posicion(Columna.I1,Fila.A)
  private val MA = Entes.Posicion(Columna.M,Fila.A)
  private val D1A = Entes.Posicion(Columna.D1,Fila.A)
  private val I2M = Entes.Posicion(Columna.I2, Fila.M)
  private val I1M = Entes.Posicion(Columna.I1,Fila.M)
  private val MM = Entes.Posicion(Columna.M,Fila.M)
  private val D1M = Entes.Posicion(Columna.D1, Fila.M)
  private val D2M = Entes.Posicion(Columna.D2, Fila.M)
  private val I1B = Entes.Posicion(Columna.I1,Fila.B)
  private val MB= Entes.Posicion(Columna.M,Fila.B)
  private val D1B= Entes.Posicion(Columna.D1,Fila.B)


  //Aqui tenemos nuestras listas de adyacencia
  private val adyacencias:Map[Entes.Posicion, Set[Entes.Posicion]] =

    Map(

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


  
  def movimientosDesde(p: Entes.Posicion): Set[Entes.Posicion] = adyacencias.getOrElse(p,Set.empty) //en caso de que no sea una posición válida


  // --- Posiciones iniciales de la liebre y sabuesos ---
  def posicionInicialLiebre: Entes.Posicion = D2M

  def posicionesInicialesSabuesos: Set[Entes.Posicion] = Set(I1A,I2M,I1B)

  def posicionMetaLiebre: Entes.Posicion = I2M


  
  // --- Pintado ---
  private def pintarNodo(p: Entes.Posicion, estado: Estado): String =

    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
    if (estado.Liebre == p) s"${ROJO}L$RESET"
    else if (estado.Sabuesos.contains(p)) s"${AZUL}S$RESET"
    else s"${BLANCO}o$RESET"


  override def pintarTablero(estado: Estado): Unit =

    val s = pintarNodo(_, estado)
    println(s"      ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println("    ╱ |  \\  |  /  | \\")
    println(s"   ${s(I2M)}--${s(I1M)}-----${s(MM)}-----${s(D1M)}--${s(D2M)}")
    println("    \\ |  /  |   \\ | /")
    println(s"      ${s(I1B)}-----${s(MB)}-----${s(D1B)}\n")



  // --- Comprueba si ha terminado la partida ---
  def esFinPartida(estado: Estado): Option[Entes.Jugador] =

    //la liebre gana si ha llegado a la posición de meta o si los sabuesos no pueden moverse más
    //Los sabuesos ganan si la liebre no tiene más posiciones a las que moverse

    if (estado.Liebre == posicionMetaLiebre || Entes.MovimientoSabueso.movimientosPosibles(this, estado).isEmpty)
    then Some(Entes.Jugador.Liebre)
    else if Entes.MovimientoLiebre.movimientosPosibles(this,estado).isEmpty
    then Some(Entes.Jugador.Sabuesos)
    else None





  //BUCLE DE JUEGO SIN HEURISTICAS

  @tailrec
  def bucleJuegoBasico(tablero: TableroJuego, estado:Estado):Entes.Jugador =

    //1.pintamos el tablero

    tablero.pintarTablero(estado)


    //2.Calculamos los mov posibles del jugador que tiene el turno

    val movimientos = estado.turno match
      case Entes.Jugador.Liebre => Entes.MovimientoLiebre.movimientosPosibles(tablero,estado)
      case Entes.Jugador.Sabuesos => Entes.MovimientoSabueso.movimientosPosiblePorSabueso(tablero,estado)


    //3.Mostramos por pantalla los movimientos del jugador que tiene el turno


    @tailrec
    def imprimirMovimientos(movimientos: Set[(Entes.Posicion, Entes.Posicion)], indiceCriatura: Int = 1, indicesYaUsados: Int = 1, listaYaSequenciados: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =
    //nos devolverá un conjunto que contiene elementos con un índice y dos posiciones, una la de partida y otra la de destino

      @tailrec
      def imprimirAux(lista: Set[Entes.Posicion], posicionBichoSiendoListado: Entes.Posicion, indiceOpcion: Int = 1, listaSiendoSequenciada: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)]  =

        if(lista.size > 1)

          val posicion = lista.head
          println(indiceOpcion + ") " + posicion.getCoordenadas())
          imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

        else if (lista.size == 1)

          println(indiceOpcion + ") " + lista.head.getCoordenadas() + "\n")
          imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

        else

          listaSiendoSequenciada


      //diferenciamos entre si se trata del turno de la liebre o de los sabuesos
      if (estado.turno == Entes.Jugador.Liebre)

        val posicionLiebre = movimientos.head._1
        println("La liebre que esta en " +  posicionLiebre.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosibles = movimientos.filter(m => m._1 == posicionLiebre).map(n => n._2)
        val indiceMovimientos = imprimirAux(posicionesPosibles, posicionLiebre)
        indiceMovimientos

      else

        if(movimientos.nonEmpty)

          val posicionSabueso = movimientos.head._1
          println("El sabueso número " + indiceCriatura + " que esta en " + posicionSabueso.getCoordenadas() + " puede moverse a:\n")
          val posicionesPosiblesParaElSabueso = movimientos.filter(m => m._1 == posicionSabueso)
          val listaAImprimir = posicionesPosiblesParaElSabueso.map(n => n._2)
          val indiceMovimientos = imprimirAux(listaAImprimir, posicionSabueso, indicesYaUsados)
          imprimirMovimientos(movimientos -- posicionesPosiblesParaElSabueso, indiceCriatura+1, indiceMovimientos.map(m => m._1).max + 1, listaYaSequenciados ++ indiceMovimientos)

        else

          listaYaSequenciados



    //Aqui tenemos nuestra lista preparada
    val listaSequenciada = imprimirMovimientos(movimientos)


    //4. Leer elección del jugador

    @tailrec
    def elegido: Int =

      try {
      //Leemos lo ingresado por el usuario
        val elegido = scala.io.StdIn.readLine().toInt

        if (listaSequenciada.exists(m => m._1 == elegido))

          elegido

        else

          throw new IllegalAccessError("\n Elija una opción válida, por favor.\n")


      } catch {
        //Con catch tratamos las excepciones para evitar fallos
        case e: IllegalAccessError =>

          println(e.getMessage)
          elegido

        case e: Exception =>

          println("\nIntroduzca un número, por favor.\n")
          elegido
      }

    val eleccion = elegido


    //Se ejecuta el movimiento y se actualiza el estado

    val datosSeleccionados = listaSequenciada.filter(m => m._1 == eleccion)
    val nuevaPos = datosSeleccionados.map(n => n._3).head//coger la lista ya sequenciada y hacer una busqueda por opcion filtrando con un map

    val nuevoEstado =

      if (estado.turno == Entes.Jugador.Liebre)

        Estado(Liebre = nuevaPos, Sabuesos = estado.Sabuesos, turno = Entes.Jugador.Sabuesos)

      else
        
        Estado(Liebre= estado.Liebre,Sabuesos = estado.Sabuesos - datosSeleccionados.head._2 + datosSeleccionados.head._3 ,turno = Entes.Jugador.Liebre)

    
    //Será fin de partida si alguno ha ganado, si no continuamos
    esFinPartida(nuevoEstado) match

      case Some(ganador) =>

        pintarTablero(nuevoEstado)
        println(s"Ha ganado: $ganador!")
        ganador

      case None => bucleJuegoBasico(tablero, nuevoEstado)








    //BUCLE DE JUEGO CON HEURISTICA DE LIEBRE



  @tailrec
  def bucleJuegoConHeuristicaLiebre(tablero: TableroJuego, estado: Estado): Entes.Jugador =

    //1.pintamos el tablero

    tablero.pintarTablero(estado)


    //2.Calculamos los mov posibles del jugador que tiene el turno

    val movimientos = estado.turno match
      case Entes.Jugador.Liebre => Entes.MovimientoLiebre.movimientosPosibles(tablero, estado)
      case Entes.Jugador.Sabuesos => Entes.MovimientoSabueso.movimientosPosiblePorSabueso(tablero, estado)


    //3.Mostramos por pantalla los movimientos del jugador que tiene el turno

    //Hay que cambiar esto para hacerlo bien, salto de linea al final de la ultima opcion y division de opciones por bicho
    //movimientos.zipWithIndex.foreach{case(mov, i) => println(s"$i,$mov")}
    @tailrec
    def imprimirMovimientos(movimientos: Set[(Entes.Posicion, Entes.Posicion)], indiceCriatura: Int = 1, indicesYaUsados: Int = 1, listaYaSequenciados: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =


      @tailrec
      def imprimirAux(lista: Set[Entes.Posicion], posicionBichoSiendoListado: Entes.Posicion, indiceOpcion: Int = 1, listaSiendoSequenciada: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =

        if(estado.turno == Liebre)

          if (lista.size > 1)

            val posicion = lista.head
            println(indiceOpcion + ") " + posicion.getCoordenadas() + " " + Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, posicion))
            imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

          else if (lista.size == 1)

            println(indiceOpcion + ") " + lista.head.getCoordenadas() + " " + Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, lista.head) +  "\n" )
            imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

          else

            listaSiendoSequenciada

        else

          if (lista.size > 1)

            val posicion = lista.head
            println(indiceOpcion + ") " + posicion.getCoordenadas())
            imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

          else if (lista.size == 1)

            println(indiceOpcion + ") " + lista.head.getCoordenadas() +  "\n" )
            imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

          else

            listaSiendoSequenciada



      if (estado.turno == Entes.Jugador.Liebre)

        val posicionLiebre = movimientos.head._1
        println("La liebre que esta en " + posicionLiebre.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosibles = movimientos.filter(m => m._1 == posicionLiebre).map(n => n._2)
        val indiceMovimientos = imprimirAux(posicionesPosibles, posicionLiebre)
        indiceMovimientos

      else if (movimientos.nonEmpty)

        val posicionSabueso = movimientos.head._1
        println("El sabueso número " + indiceCriatura + " que esta en " + posicionSabueso.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosiblesParaElSabueso = movimientos.filter(m => m._1 == posicionSabueso)
        val listaAImprimir = posicionesPosiblesParaElSabueso.map(n => n._2)
        val indiceMovimientos = imprimirAux(listaAImprimir, posicionSabueso, indicesYaUsados)
        imprimirMovimientos(movimientos -- posicionesPosiblesParaElSabueso, indiceCriatura + 1, indiceMovimientos.map(m => m._1).max + 1, listaYaSequenciados ++ indiceMovimientos)

      else

        listaYaSequenciados


    val listaSequenciada = imprimirMovimientos(movimientos)


    //4. Leer elección del jugador

    @tailrec
    def elegido: Int =

      try {


        val elegido = scala.io.StdIn.readLine().toInt

        if (listaSequenciada.exists(m => m._1 == elegido))

          elegido

        else

          throw new IllegalAccessError("\n Elija una opción válida, por favor.\n")


      } catch {

        case e: IllegalAccessError =>

          println(e.getMessage)
          elegido

        case e: Exception =>

          println("\nIntroduzca un número, por favor.\n")
          elegido
          
      }

    val eleccion = elegido




    //Se ejecuta el movimiento y se actualiza el estado

    val datosSeleccionados = listaSequenciada.filter(m => m._1 == eleccion)
    val nuevaPos = datosSeleccionados.map(n => n._3).head //coger la lista ya sequenciada y hacer una busqueda por opcion filtrando con un map

    val nuevoEstado =

      if (estado.turno == Entes.Jugador.Liebre)

        Estado(Liebre = nuevaPos, Sabuesos = estado.Sabuesos, turno = Entes.Jugador.Sabuesos)

      else

        Estado(Liebre = estado.Liebre, Sabuesos = estado.Sabuesos - datosSeleccionados.head._2 + datosSeleccionados.head._3, turno = Entes.Jugador.Liebre)


    esFinPartida(nuevoEstado) match

      case Some(ganador) =>

        pintarTablero(nuevoEstado)
        println(s"Ha ganado: $ganador!")
        ganador

      case None => bucleJuegoConHeuristicaLiebre(tablero, nuevoEstado)







    //BUCLE DE JUEGO CON IA DE LA LIEBRE

  @tailrec
  def bucleJuegoConIALiebre(tablero: TableroJuego, estado: Estado, IALiebre: Boolean): Entes.Jugador =

    //1.pintamos el tablero

    tablero.pintarTablero(estado)


    //2.Calculamos los mov posibles del jugador que tiene el turno

    val movimientos = estado.turno match
      case Entes.Jugador.Liebre => Entes.MovimientoLiebre.movimientosPosibles(tablero, estado)
      case Entes.Jugador.Sabuesos => Entes.MovimientoSabueso.movimientosPosiblePorSabueso(tablero, estado)


    //3.Mostramos por pantalla los movimientos del jugador que tiene el turno


    @tailrec
    def imprimirMovimientos(movimientos: Set[(Entes.Posicion, Entes.Posicion)], indiceCriatura: Int = 1, indicesYaUsados: Int = 1, listaYaSequenciados: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =


      @tailrec
      def imprimirAux(lista: Set[Entes.Posicion], posicionBichoSiendoListado: Entes.Posicion, indiceOpcion: Int = 1, listaSiendoSequenciada: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =

        if(estado.turno == Liebre)

          if (lista.size > 1)

            val posicion = lista.head
            println(indiceOpcion + ") " + posicion.getCoordenadas() + " " + Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, posicion))
            imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

          else if (lista.size == 1)

            println(indiceOpcion + ") " + lista.head.getCoordenadas() + " " + Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, lista.head) +  "\n" )
            imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

          else

            listaSiendoSequenciada

        else

          if (lista.size > 1)

            val posicion = lista.head
            println(indiceOpcion + ") " + posicion.getCoordenadas())
            imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

          else if (lista.size == 1)

            println(indiceOpcion + ") " + lista.head.getCoordenadas() +  "\n" )
            imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

          else

            listaSiendoSequenciada



      if (estado.turno == Entes.Jugador.Liebre)

        val posicionLiebre = movimientos.head._1
        println("La liebre que esta en " + posicionLiebre.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosibles = movimientos.filter(m => m._1 == posicionLiebre).map(n => n._2)
        val indiceMovimientos = imprimirAux(posicionesPosibles, posicionLiebre)
        indiceMovimientos

      else if (movimientos.nonEmpty)

        val posicionSabueso = movimientos.head._1
        println("El sabueso número " + indiceCriatura + " que esta en " + posicionSabueso.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosiblesParaElSabueso = movimientos.filter(m => m._1 == posicionSabueso)
        val listaAImprimir = posicionesPosiblesParaElSabueso.map(n => n._2)
        val indiceMovimientos = imprimirAux(listaAImprimir, posicionSabueso, indicesYaUsados)
        imprimirMovimientos(movimientos -- posicionesPosiblesParaElSabueso, indiceCriatura + 1, indiceMovimientos.map(m => m._1).max + 1, listaYaSequenciados ++ indiceMovimientos)

      else

        listaYaSequenciados


    val listaSequenciada = imprimirMovimientos(movimientos)


    //4. Leer elección del jugador

    @tailrec
    def eleccion: Int =

      if(IALiebre && estado.turno == Liebre)

        if(Heuristicas.MovimientoLiebre.algunSabuesoHaSidoRebasado(tablero, estado))


          try{
          //Necesitamos sleep para dar tiempo al usuario a ver lo que ocurre y que la IA no actúe inmediatamente
            Thread.sleep(800)

          }catch {
            case e: Throwable =>

            InterruptedException("Ha habido un problema técnico, el juego se cerrará. \n")
            sys.exit(1)

          }

          val posicionesDisponibles = listaSequenciada.map{case (x, y , z) => (x, y, Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, z))}
          val maximosHeuristica = posicionesDisponibles.filter {case (x, y, z) => z._1 == posicionesDisponibles.map { case (x, y, z) => z._1 }.min}

          if(maximosHeuristica.size > 1) {

            //En caso de empate en el segundo elemento de la heuristica coge uno al azar
            Random.shuffle(maximosHeuristica.filter{case (x, y, z) => z._1 == maximosHeuristica.map{ case (x, y, z) => z._1}.max}).head._1

          } else

            maximosHeuristica.head._1

        else

          val posicionesDisponibles = listaSequenciada.map{case (x, y , z) => (x, y, Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, z))}
          val maximosHeuristica = posicionesDisponibles.filter {case (x, y, z) => z._1 == posicionesDisponibles.map { case (x, y, z) => z._1 }.max}

          if(maximosHeuristica.size > 1) {

            //En caso de empate en el segundo elemento de la heuristica coge uno al azar
            Random.shuffle(maximosHeuristica.filter{case (x, y, z) => z._1 == maximosHeuristica.map{ case (x, y, z) => z._1}.max}).head._1

          } else

            maximosHeuristica.head._1


      else

        try {

          val elegido = scala.io.StdIn.readLine().toInt

          if (listaSequenciada.exists(m => m._1 == elegido))

            elegido

          else

            throw new IllegalAccessError("\n Elija una opción válida, por favor.\n")


        } catch {

          case e: IllegalAccessError =>

            println(e.getMessage)
            eleccion

          case e: Exception =>

            println("\nIntroduzca un número, por favor.\n")
            eleccion

        }


    //Se ejecuta el movimiento y se actualiza el estado

    val opcionElegida = eleccion
    val datosSeleccionados = listaSequenciada.filter(m => m._1 == opcionElegida)
    val nuevaPos = datosSeleccionados.map(n => n._3).head //coger la lista ya sequenciada y hacer una busqueda por opcion filtrando con un map

    val nuevoEstado =

      if (estado.turno == Entes.Jugador.Liebre)

        Estado(nuevaPos, estado.Sabuesos, Entes.Jugador.Sabuesos)

      else

        Estado(estado.Liebre, estado.Sabuesos - datosSeleccionados.head._2 + datosSeleccionados.head._3, Entes.Jugador.Liebre)


    esFinPartida(nuevoEstado) match

      case Some(ganador) =>

        pintarTablero(nuevoEstado)
        println(s"Ha ganado: $ganador!")
        ganador

      case None => bucleJuegoConIALiebre(tablero, nuevoEstado, IALiebre)



  @tailrec
  def bucleJuegoConIADoble(tablero: TableroJuego, estado: Estado, modoIA: Set[Jugador]): Entes.Jugador =

    //1.pintamos el tablero

    tablero.pintarTablero(estado)


    //2.Calculamos los mov posibles del jugador que tiene el turno

    val movimientos = estado.turno match
      case Entes.Jugador.Liebre => Entes.MovimientoLiebre.movimientosPosibles(tablero, estado)
      case Entes.Jugador.Sabuesos => Entes.MovimientoSabueso.movimientosPosiblePorSabueso(tablero, estado)


    //3.Mostramos por pantalla los movimientos del jugador que tiene el turno

    //Hay que cambiar esto para hacerlo bien, salto de linea al final de la ultima opcion y division de opciones por bicho
    @tailrec
    def imprimirMovimientos(movimientos: Set[(Entes.Posicion, Entes.Posicion)], indiceCriatura: Int = 1, indicesYaUsados: Int = 1, listaYaSequenciados: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =


      @tailrec
      def imprimirAux(lista: Set[Entes.Posicion], posicionBichoSiendoListado: Entes.Posicion, indiceOpcion: Int = 1, listaSiendoSequenciada: Set[(Int, Entes.Posicion, Entes.Posicion)] = Set()): Set[(Int, Entes.Posicion, Entes.Posicion)] =

        if (estado.turno == Liebre)

          if (lista.size > 1)

            val posicion = lista.head
            println(s"$indiceOpcion) " + posicion.getCoordenadas() + " " + Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, posicion).toStringBonitoLiebre)
            imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

          else if (lista.size == 1)

            println(s"$indiceOpcion) " + lista.head.getCoordenadas() + " " + Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, lista.head).toStringBonitoLiebre + "\n")
            imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

          else

            listaSiendoSequenciada

        else if (lista.size > 1)

          val posicion = lista.head
          println(s"$indiceOpcion) " + posicion.getCoordenadas() + " " + Heuristicas.MovimientoSabuesos.evaluarMovimiento(tablero, estado, posicionBichoSiendoListado, posicion).toStringBonitoSabuesos)
          imprimirAux(lista - posicion, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, posicion)))

        else if (lista.size == 1)

          println(s"$indiceOpcion) " + lista.head.getCoordenadas() + " " + Heuristicas.MovimientoSabuesos.evaluarMovimiento(tablero, estado, posicionBichoSiendoListado, lista.head).toStringBonitoSabuesos + "\n")
          imprimirAux(lista - lista.head, posicionBichoSiendoListado, indiceOpcion + 1, listaSiendoSequenciada ++ Set((indiceOpcion, posicionBichoSiendoListado, lista.head)))

        else

          listaSiendoSequenciada



      if (estado.turno == Entes.Jugador.Liebre)

        val posicionLiebre = movimientos.head._1
        println("La liebre que esta en " + posicionLiebre.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosibles = movimientos.filter(m => m._1 == posicionLiebre).map(n => n._2)
        val indiceMovimientos = imprimirAux(posicionesPosibles, posicionLiebre)
        indiceMovimientos

      else if (movimientos.nonEmpty)

        val posicionSabueso = movimientos.head._1
        println("El sabueso número " + indiceCriatura + " que esta en " + posicionSabueso.getCoordenadas() + " puede moverse a:\n")
        val posicionesPosiblesParaElSabueso = movimientos.filter(m => m._1 == posicionSabueso)
        val listaAImprimir = posicionesPosiblesParaElSabueso.map(n => n._2)
        val indiceMovimientos = imprimirAux(listaAImprimir, posicionSabueso, indicesYaUsados)
        imprimirMovimientos(movimientos -- posicionesPosiblesParaElSabueso, indiceCriatura + 1, indiceMovimientos.map(m => m._1).max + 1, listaYaSequenciados ++ indiceMovimientos)

      else

        listaYaSequenciados


    val listaSequenciada = imprimirMovimientos(movimientos)


    //4. Leer elección del jugador

    @tailrec
    def eleccion: Int =

      if (estado.turno == Liebre && modoIA.contains(Entes.Jugador.Liebre))

        if (Heuristicas.MovimientoLiebre.algunSabuesoHaSidoRebasado(tablero, estado))


          try {

            Thread.sleep(800)

          } catch {

            case e: Throwable =>

              InterruptedException("Ha habido un problema técnico, el juego se cerrará. \n")
              sys.exit(1)

          }

          val posicionesDisponibles = listaSequenciada.map { case (x, y, z) => (x, y, Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, z)) }
          val maximosHeuristica = posicionesDisponibles.filter { case (x, y, z) => z._1 == posicionesDisponibles.map { case (x, y, z) => z._1 }.min }

          if (maximosHeuristica.size > 1) {

            //En caso de empate en el segundo elemento de la heuristica coge uno al azar
            Random.shuffle(maximosHeuristica.filter { case (x, y, z) => z._1 == maximosHeuristica.map { case (x, y, z) => z._1 }.max }).head._1

          } else

            maximosHeuristica.head._1

        else

          val posicionesDisponibles = listaSequenciada.map { case (x, y, z) => (x, y, Heuristicas.MovimientoLiebre.evaluarMovimiento(tablero, estado, z)) }
          val maximosHeuristica = posicionesDisponibles.filter { case (x, y, z) => z._1 == posicionesDisponibles.map { case (x, y, z) => z._1 }.max }

          if (maximosHeuristica.size > 1) {

            //En caso de empate en el segundo elemento de la heuristica coge uno al azar
            Random.shuffle(maximosHeuristica.filter { case (x, y, z) => z._1 == maximosHeuristica.map { case (x, y, z) => z._1 }.max }).head._1

          } else

            maximosHeuristica.head._1

      else if (estado.turno == Sabuesos && modoIA.contains(Entes.Jugador.Sabuesos)) {

        try {

          Thread.sleep(800)

        } catch {

          case e: Throwable =>

            InterruptedException("Ha habido un problema técnico, el juego se cerrará. \n")
            sys.exit(1)

        }

        val posicionesPosiblesSabuesos = listaSequenciada.map { case (x, y, z) => (x, y, Heuristicas.MovimientoSabuesos.evaluarMovimiento(tablero, estado, y, z)) }
        val posicionesDondeNoHayRebasamiento = posicionesPosiblesSabuesos.filter{case (x, y, z) => !z._1}
        val posicionesConPosibleRebasamiento = posicionesPosiblesSabuesos -- posicionesDondeNoHayRebasamiento



        def selectorMejorOpcion(opciones:  Set[(Int, Entes.Posicion, (Boolean, Int, Int))]): Int = {

          def selectorOpcionesAux(opcionesDelSabuesoOptimo: Set[(Int, Entes.Posicion, (Boolean, Int, Int))]): Int =

            val opcionesQueSeAcercanALaLiebre = opcionesDelSabuesoOptimo.filter { case (x, y, z) => z._2 == opcionesDelSabuesoOptimo.map { case (x, y, z) => z._2 }.min }

            if (opcionesQueSeAcercanALaLiebre.size > 1) {

              val opcionesQueAdemasMinimizan = opcionesQueSeAcercanALaLiebre.filter { case (x, y, z) => z._3 == opcionesQueSeAcercanALaLiebre.map { case (x, y, z) => z._3 }.max }

              if (opcionesQueAdemasMinimizan.size > 1)

                Random.shuffle(opcionesQueAdemasMinimizan).head._1

              else

                opcionesQueAdemasMinimizan.head._1

            }

            else

              opcionesQueSeAcercanALaLiebre.head._1




          val sabuesosEntreLosQueElegimos = opciones.map(m => m._2)

          if(sabuesosEntreLosQueElegimos.size > 1)

            val maximoDistancia = sabuesosEntreLosQueElegimos.map(m => FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(m, estado.Liebre)).max
            val sabuesoMasLejosDeLaLiebre = sabuesosEntreLosQueElegimos.filter(f => FuncionesGeneralesHeuristicas.distanciaEnteAObjetivo(f, estado.Liebre) == maximoDistancia)

            val sabuesoElegido = Random.shuffle(sabuesoMasLejosDeLaLiebre).head
            val opcionesDelSabuesoElegido = opciones.filter { case (x, y, z) => y == sabuesoElegido }

            selectorOpcionesAux(opcionesDelSabuesoElegido)

          else

            selectorOpcionesAux(opciones)


        }


        if(posicionesDondeNoHayRebasamiento.isEmpty) {

          selectorMejorOpcion(posicionesConPosibleRebasamiento)

        } else

          selectorMejorOpcion(posicionesDondeNoHayRebasamiento)


      } else

        try {

          val elegido = scala.io.StdIn.readLine().toInt

          if (listaSequenciada.exists(m => m._1 == elegido))

            elegido

          else

            throw new IllegalAccessError("\n Elija una opción válida, por favor.\n")


        }catch {

          case e: IllegalAccessError =>

            println(e.getMessage)
            eleccion

          case e: Exception =>

            println("\nIntroduzca un número, por favor.\n")
            eleccion

        }


    //Se ejecuta el movimiento y se actualiza el estado

    val opcionElegida = eleccion
    val datosSeleccionados = listaSequenciada.filter(m => m._1 == opcionElegida)

    val nuevaPos = datosSeleccionados.map(n => n._3).head //coger la lista ya sequenciada y hacer una busqueda por opcion filtrando con un map

    val nuevoEstado =

      if (estado.turno == Entes.Jugador.Liebre)

        Estado(nuevaPos, estado.Sabuesos, Entes.Jugador.Sabuesos)

      else

        Estado(estado.Liebre, estado.Sabuesos - datosSeleccionados.head._2 + datosSeleccionados.head._3, Entes.Jugador.Liebre)


    esFinPartida(nuevoEstado) match

      case Some(ganador) =>

        pintarTablero(nuevoEstado)
        println(s"Ha ganado: $ganador!")
        ganador

      case None => bucleJuegoConIADoble(tablero, nuevoEstado, modoIA)
