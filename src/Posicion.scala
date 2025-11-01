case class Posicion(col: Columna, fila: Fila):
  def x:Int = col.valor
  def y:Int = fila.valor
