package risk.shared

import risk.shared.abstract0.{AbstractRiskPlayerInfo, RiskColor}

case class ClientRiskPlayerInfo(
    override val troops: Int,
    override val eliminated: Boolean,
    override val rollDefenderDieAutomatically: Boolean,
    override val rollAttackerDieAutomatically: Boolean,
    override val defendWithOneAutomaticallyIfOnlyOneUnitIsPresent: Boolean,
    inventory: Either[Int, List[String]]
) extends AbstractRiskPlayerInfo(
      troops,
      eliminated,
      rollDefenderDieAutomatically,
      rollAttackerDieAutomatically,
      defendWithOneAutomaticallyIfOnlyOneUnitIsPresent
    ) {
  val inventorySize: Int = inventory match {
    case Left(number) => number
    case Right(list)  => list.size
  }
}
