package Entorno

trait TableroJuego:
  
  // --- Devuelve las posiciones accesibles desde una posición ---
  def movimientosDesde(p: Entes.Posicion): Set[Entes.Posicion]

  // --- Posiciones iniciales de la liebre y sabuesos ---
  def posicionInicialLiebre: Entes.Posicion

  def posicionesInicialesSabuesos: Set[Entes.Posicion]

  def posicionMetaLiebre: Entes.Posicion

  // --- Pinta el tablero para un estado dado
  def pintarTablero(estado: Estado): Unit

  // --- Comprueba si ha terminado la partida ---
  def esFinPartida(estado: Estado): Option[Entes.Jugador]

