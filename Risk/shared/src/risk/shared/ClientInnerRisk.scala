package risk.shared

import risk.shared.abstract0._
import upickle.default.{macroRW, ReadWriter => RW}

case class ClientInnerRisk(
                            override val territories: List[TerritoryOccupationInfo],
                            override val players: List[ClientRiskPlayer],
                            override val currentPlayer: Int,
                            override val phase: GamePhase,
                            override val campaign: Option[ClientCampaign] = None,
                            override val round: Int,
                            override val noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer: Int
                          ) extends AbstractInnerRisk

object ClientInnerRisk {
  implicit def rw: RW[ClientInnerRisk] = macroRW
}
