package Entes

case class Posicion(columna: Entorno.Columna, fila: Entorno.Fila):
  
  def x:Int = columna.valor
  def y:Int = fila.valor
  
  def getCoordenadas(posicion: Posicion = this): String =

    fila.toString + columna.toString
  

  def getX: Int = x
  def getY: Int = y