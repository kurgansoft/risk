package risk.shared.abstract0

sealed trait GamePhase

object GamePhase {
  case object AddingUnits extends GamePhase
  case object DeployingUnits extends GamePhase
  case object BattleAndManeuver extends GamePhase
  case object GameOver extends GamePhase
}
