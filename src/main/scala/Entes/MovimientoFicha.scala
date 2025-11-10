package Entes

import Entorno.{Estado, TableroJuego}


sealed trait MovimientoFicha:
  
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)]



//Aqui tenemos que Movimientos Posibles para la liebre nos devuelve un conjunto con 
// elementos de tipo (Posicion,Posicion), uno de los cambios que hemos realizado con respecto a la guía
case object MovimientoLiebre extends MovimientoFicha:
  
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =

    (tablero.movimientosDesde(estado.Liebre) -- estado.Sabuesos).map(m => (estado.Liebre, m))
    


case object MovimientoSabueso extends MovimientoFicha:
  
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =

    movimientosPosiblePorSabueso(tablero, estado)    

  def movimientosPosiblePorSabueso(tablero:TableroJuego, estado:Estado): Set[(Posicion,Posicion)] = 
    
    //Creamos una función auxiliar para obtener los pares de cada sabueso 
    def aux(pos: Posicion): Set[(Posicion, Posicion)] = 
      
      val movimientos = tablero.movimientosDesde(pos).filter(p => p.x >= pos.x)
      val movSinOcupados = movimientos -- estado.ocupadas
      movSinOcupados.map(dest => (pos, dest))
      
    
    
    //devolvemos el Set de pares de cada sabueso con sus posibles movimientos
    estado.Sabuesos.flatMap(s => aux(s))

  
    
