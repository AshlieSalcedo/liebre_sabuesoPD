import Entes.Jugador.{Liebre, Sabuesos}
import Entorno.{Estado, TableroClasicoLyS}

import scala.annotation.tailrec

object LiebreYSabueso extends App {


  
  
  bucleDelPrograma()
  
  
  
  
  private def bucleDelPrograma(): Unit = {


    println("\n Bienvenido al juego de la liebre y los sabuesos. \n")

    selector1()

  }






  @tailrec
  private def selector1(): Unit =

    println("¿Quieres jugar con la IA? S/N\n")
    val seleccionDelUsuario = scala.io.StdIn.readLine()

    seleccionDelUsuario match {

      case "S" | "s" =>

        selector2()

      case "N" | "n" =>

        selector4()

      case _ =>

        println("\nPor favor, introduzca una opción válida.\n")
        selector1()
    }









  @tailrec
  private def selector4(): Unit =

    println("¿Quieres jugar con pistas para la liebre? S/N\n")
    val seleccionDelUsuario4 = scala.io.StdIn.readLine()

    seleccionDelUsuario4 match {

      case "S" | "s" =>

        jugar1()
        selector5()

        def jugar1(): Unit =

          //INVOCACION BUCLE DE JUEGO CON HEURISTICA DE LIEBRE

          TableroClasicoLyS.bucleJuegoConHeuristicaLiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))


        @tailrec
        def selector5(): Unit =

          println("\n¿Quieres jugar otra partida? S/N\n")
          val seleccionDelUsuario5 = scala.io.StdIn.readLine()

          seleccionDelUsuario5 match {

            case "S" | "s" =>

              @tailrec
              def selector6(): Unit =

                println("¿Quieres jugar con las mismas opciones? S/N\n")
                val seleccionDelUsuario6 = scala.io.StdIn.readLine()

                seleccionDelUsuario6 match {

                  case "S" | "s" => jugar1()
                  case "N" | "n" => selector1()
                  case _ =>

                    println("\nPor favor, introduzca una opción válida.\n")
                    selector6()

                }

              selector6()

            case "N" | "n" => sys.exit(0)

            case _ =>

              println("\nPor favor, introduzca una opción válida.\n")
              selector5()

          }


      case "N" | "n" => {

        jugar2()
        selector7()

        def jugar2(): Unit =

          //INVOCACION BUCLE DE JUEGO SIN HEURISTICAS

          TableroClasicoLyS.bucleJuegoBasico(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()))



        @tailrec
        def selector7(): Unit =

          println("\n¿Quieres jugar otra partida? S/N\n")
          val seleccionDelUsuario7 = scala.io.StdIn.readLine()

          seleccionDelUsuario7 match {

            case "S" | "s" =>

              @tailrec
              def selector8(): Unit =

                println("¿Quieres jugar con las mismas opciones? S/N\n")
                val seleccionDelUsuario8 = scala.io.StdIn.readLine()

                seleccionDelUsuario8 match {

                  case "S" | "s" => jugar2()
                  case "N" | "n" => selector1()
                  case _ =>

                    println("\nPor favor, introduzca una opción válida.\n")
                    selector8()

                }

              selector7()

            case "N" | "n" => sys.exit(0)

            case _ =>

              println("\nPor favor, introduzca una opción válida.\n")
              selector7()

          }

      }

      case _ =>

        println("\nPor favor, introduzca una opción válida.\n")
        selector4()


    }








  @tailrec
  private def selector2(): Unit =

    println("¿Quieres que juegue sola la IA? S/N\n")
    val seleccionDeUsuario2 = scala.io.StdIn.readLine()

    seleccionDeUsuario2 match {

      case "S" | "s" =>

        jugar3()
        selector9()

        def jugar3(): Unit =

          //INVOCACION BUCLE DE JUEGO CON IA DOBLE

          TableroClasicoLyS.bucleJuegoConIADoble(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()), Set(Sabuesos, Liebre))


        @tailrec
        def selector9(): Unit =

          println("\n¿Quieres jugar otra partida? S/N\n")
          val seleccionDelUsuario9 = scala.io.StdIn.readLine()

          seleccionDelUsuario9 match {

            case "S" | "s" =>

              @tailrec
              def selector10(): Unit =

                println("¿Quieres jugar con las mismas opciones? S/N\n")
                val seleccionDelUsuario10 = scala.io.StdIn.readLine()

                seleccionDelUsuario10 match {

                  case "S" | "s" => jugar3()
                  case "N" | "n" => selector1()
                  case _ =>

                    println("\nPor favor, introduzca una opción válida.\n")
                    selector10()

                }

              selector9()

            case "N" | "n" => sys.exit(0)

            case _ =>

              println("\nPor favor, introduzca una opción válida.\n")
              selector9()

          }


      case "N" | "n" =>

        selector3()


      case _ =>

        println("\nPor favor, introduzca una opción válida.\n")
        selector2()
    }









  @tailrec
  private def selector3(): Unit =

    println("¿Quieres jugar de Sabueso? S/N\n")
    val seleccionDeUsuario3 = scala.io.StdIn.readLine()

    seleccionDeUsuario3 match {

      case "S" | "s" =>

        jugar4()
        selector11()

        def jugar4(): Unit =

          //INVOCACION BUCLE DE JUEGO CON IA LIEBRE

          TableroClasicoLyS.bucleJuegoConIALiebre(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()), true)


        @tailrec
        def selector11(): Unit =

          println("\n¿Quieres jugar otra partida? S/N\n")
          val seleccionDelUsuario11 = scala.io.StdIn.readLine()

          seleccionDelUsuario11 match {

            case "S" | "s" =>

              @tailrec
              def selector12(): Unit =

                println("¿Quieres jugar con las mismas opciones? S/N\n")
                val seleccionDelUsuario12 = scala.io.StdIn.readLine()

                seleccionDelUsuario12 match {

                  case "S" | "s" => jugar4()
                  case "N" | "n" => selector1()
                  case _ =>

                    println("\nPor favor, introduzca una opción válida.\n")
                    selector12()

                }

              selector11()

            case "N" | "n" => sys.exit(0)

            case _ =>

              println("\nPor favor, introduzca una opción válida.\n")
              selector11()

          }


      case "N" | "n" =>

        jugar5()
        selector13()


        def jugar5(): Unit =

          //INVOCACION BUCLE DE JUEGO CON IA SABUESO

          TableroClasicoLyS.bucleJuegoConIADoble(TableroClasicoLyS, Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, Estado.sortearTurno()), Set(Sabuesos))


        @tailrec
        def selector13(): Unit =

          println("\n¿Quieres jugar otra partida? S/N\n")
          val seleccionDelUsuario13 = scala.io.StdIn.readLine()

          seleccionDelUsuario13 match {

            case "S" | "s" =>

              @tailrec
              def selector14(): Unit =

                println("¿Quieres jugar con las mismas opciones? S/N\n")
                val seleccionDelUsuario14 = scala.io.StdIn.readLine()

                seleccionDelUsuario14 match {

                  case "S" | "s" => jugar5()
                  case "N" | "n" => selector1()
                  case _ =>

                    println("\nPor favor, introduzca una opción válida.\n")
                    selector14()

                }

              selector13()

            case "N" | "n" => sys.exit(0)

            case _ =>

              println("\nPor favor, introduzca una opción válida.\n")
              selector13()

          }


      case _ =>

        println("\nPor favor, introduzca una opción válida.\n")
        selector3()

    }




}