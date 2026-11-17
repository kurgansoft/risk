package risk.backend

import risk.shared.ClientRiskPlayerInfo
import risk.shared.abstract0.{AbstractRiskPlayerInfo, RiskColor}
import zio.blocks.schema.Schema

case class RiskPlayerInfo(
    override val troops: Int,
    override val eliminated: Boolean = false,
    override val rollDefenderDieAutomatically: Boolean = false,
    override val rollAttackerDieAutomatically: Boolean = false,
    override val defendWithOneAutomaticallyIfOnlyOneUnitIsPresent: Boolean =
      false,
    inventory: List[String] = List.empty
) extends AbstractRiskPlayerInfo(
      troops,
      eliminated,
      rollDefenderDieAutomatically,
      rollAttackerDieAutomatically,
      defendWithOneAutomaticallyIfOnlyOneUnitIsPresent
    ) {

  lazy val toOwnersClientRiskPlayer: ClientRiskPlayerInfo =
    ClientRiskPlayerInfo(
      troops,
      eliminated,
      rollDefenderDieAutomatically,
      rollAttackerDieAutomatically,
      defendWithOneAutomaticallyIfOnlyOneUnitIsPresent,
      Right(inventory)
    )

  lazy val toClientRiskPlayer: ClientRiskPlayerInfo =
    ClientRiskPlayerInfo(
      troops,
      eliminated,
      rollDefenderDieAutomatically,
      rollAttackerDieAutomatically,
      defendWithOneAutomaticallyIfOnlyOneUnitIsPresent,
      Left(inventory.size)
    )
}

object RiskPlayerInfo {
  implicit val schema: Schema[RiskPlayerInfo] = Schema.derived
}
