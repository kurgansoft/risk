package risk.backend

import risk.shared.ClientRiskPlayer
import risk.shared.abstract0.{AbstractRiskPlayer, RiskColor}

case class RiskPlayer(
                       override val role: Int,
                       override val color: RiskColor,
                       override val troops: Int,
                       override val eliminated: Boolean = false,
                       override val rollDefenderDieAutomatically: Boolean = false,
                       override val rollAttackerDieAutomatically: Boolean = false,
                       override val defendWithOneAutomaticallyIfOnlyOneUnitIsPresent: Boolean = false,
                       inventory: List[String] = List.empty,
                     ) extends AbstractRiskPlayer(role, color, troops, eliminated, rollDefenderDieAutomatically, rollAttackerDieAutomatically, defendWithOneAutomaticallyIfOnlyOneUnitIsPresent) {

  lazy val toOwnersClientRiskPlayer: ClientRiskPlayer =
    ClientRiskPlayer(role, color, troops, eliminated, rollDefenderDieAutomatically, rollAttackerDieAutomatically, defendWithOneAutomaticallyIfOnlyOneUnitIsPresent, Right(inventory))

  lazy val toClientRiskPlayer: ClientRiskPlayer =
    ClientRiskPlayer(role, color, troops, eliminated, rollDefenderDieAutomatically, rollAttackerDieAutomatically, defendWithOneAutomaticallyIfOnlyOneUnitIsPresent, Left(inventory.size))
}

