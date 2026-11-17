package risk.shared.abstract0

import gbge.shared.GameState.{IN_PROGRESS, NOT_STARTED}
import gbge.shared.{Game, GameState}

import scala.util.Try

abstract class AbstractRisk(
    val noOfPlayers: Int = 3,
    val innerRisk: Option[AbstractInnerRisk] = None,
    override val activeRoles: Set[RiskRole] = Set.empty
) extends Game[RiskRole, RiskAction] {
  override val minPlayerNumber: Int = 3
  override val maxPlayerNumber: Int = 5

  override lazy val state: GameState =
    if (innerRisk.isDefined) IN_PROGRESS else NOT_STARTED

  override val allPossibleRoles: Set[RiskRole] = RiskRole.allRole

  def readyToStart: Boolean = {
    state == GameState.NOT_STARTED && activeRoles.size == noOfPlayers
  }
}
