package risk.backend

import gbge.backend.{BackendGame, BackendGameProps}
import gbge.shared.actions.{Action, NaturalLink}
import risk.shared.{ClientRisk, RiskProps}
import risk.shared.abstract0.{ColorMap, RiskAction, RiskRole}

object BackendRiskProps
    extends RiskProps
    with BackendGameProps[RiskRole, RiskAction, ClientRisk] {

  override def start(
      noOfPlayers: Int
  ): (BackendGame[RiskRole, RiskAction, ClientRisk], Option[Action]) = {
    val z = noOfPlayers match {
      case x if x <= 3 => 3
      case 4           => 4
      case _           => 5
    }
    (Risk(z), None)
  }
}
