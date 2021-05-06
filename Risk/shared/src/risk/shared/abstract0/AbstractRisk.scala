package risk.shared.abstract0

import gbge.shared.{Game, GameState, NOT_STARTED}

abstract class AbstractRisk(
                             val noOfPlayers: Int = 3,
                             val colorMap: ColorMap = ColorMap(),
                             val state: GameState = NOT_STARTED,
                             val innerRisk: Option[AbstractInnerRisk] = None
                           ) extends Game {
  override val minPlayerNumber: Int = 3
  override val maxPlayerNumber: Int = 5

  override val roles: List[RiskRole] = (1 to noOfPlayers).toList.map(RiskRole)


  def readyToStart: Boolean = {
    state == NOT_STARTED && colorMap.colorMap.size == noOfPlayers //&& roles.size == noOfPlayers
  }
}