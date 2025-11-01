sealed trait MovimientoFicha:
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
  
    tablero.movimientosDesde(estado.Liebre) -- estado.Sabuesos
case object MovimientoSabueso extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    (estado.Sabuesos.flatMap { s =>
      tablero.movimientosDesde(s).filter(p => p.x >= s.x)
    }) -- estado.ocupadas
  def movimientosPosiblePorSabueso(tablero:TableroJuego, estado:Estado): Set [(Posicion,Posicion)]=
    estado.Sabuesos.flatMap{s=> movimientosPosibles(tablero, estado).filter(p=>tablero.movimientosDesde(s).contains(p)).map(p=>(s,p))}
    
