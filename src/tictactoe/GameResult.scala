package tictactoe

sealed trait GameResult

case object Draw extends GameResult
case class Win(p: Player) extends GameResult
case class OnGoing(nextPlayer: Player) extends GameResult
