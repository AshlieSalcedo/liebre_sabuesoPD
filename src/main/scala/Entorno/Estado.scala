package Entorno

import scala.util.Random


case class Estado(Liebre: Entes.Posicion, Sabuesos: Set[Entes.Posicion],turno: Entes.Jugador):
  
  def ocupadas: Set[Entes.Posicion] =  Sabuesos + Liebre
//creamos un companion object para poder crear instancias 
// y agrupar funciones auxiliares  
  
object Estado:
  
  //Nuestra función para decidir quién empieza
  def sortearTurno(): Entes.Jugador =
    
    if Random.nextBoolean() 
    then Entes.Jugador.Liebre
    else Entes.Jugador.Sabuesos
    
  