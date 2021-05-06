package risk.shared

import risk.shared.abstract0.{AbstractRiskPlayer, RiskColor}
import upickle.default.{macroRW, ReadWriter => RW}

case class ClientRiskPlayer(
                       override val role: Int,
                       override val color: RiskColor,
                       override val troops: Int,
                       override val eliminated: Boolean,
                       override val rollDefenderDieAutomatically: Boolean,
                       override val rollAttackerDieAutomatically: Boolean,
                       override val defendWithOneAutomaticallyIfOnlyOneUnitIsPresent: Boolean,
                       inventory: Either[Int, List[String]]
                     ) extends AbstractRiskPlayer(role, color, troops, eliminated, rollDefenderDieAutomatically, rollAttackerDieAutomatically, defendWithOneAutomaticallyIfOnlyOneUnitIsPresent) {
  val inventorySize: Int = inventory match {
    case Left(number) => number
    case Right(list) => list.size
  }
}

object ClientRiskPlayer {
  implicit def rw: RW[ClientRiskPlayer] = macroRW
}