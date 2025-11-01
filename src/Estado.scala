import scala.util.Random
case class Estado(Liebre: Posicion, Sabuesos: Set[Posicion],turno:Jugador):
  def ocupadas: Set[Posicion] = Sabuesos + Liebre
//creamos un companion object para poder crear instancias 
// y agrupar funciones auxiliares  
object Estado:
//Nuestra función para decidir quién empieza
  def sortearTurno(): Jugador =
    if Random.nextBoolean() then Jugador.Liebre 
    else Jugador.Sabuesos
  