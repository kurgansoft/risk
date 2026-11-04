package risk.backend

import gbge.backend.{BackendGame, BackendGameProps}
import gbge.shared.actions.{Action, NaturalLink}
import risk.shared.{ClientRisk, RiskProps}
import risk.shared.abstract0.{ColorMap, RiskAction}

object BackendRiskProps
    extends RiskProps
    with BackendGameProps[RiskAction, ClientRisk] {

  override def start(
      noOfPlayers: Int
  ): (BackendGame[RiskAction, ClientRisk], Option[Action]) = {
    val z = noOfPlayers match {
      case x if x <= 3 => 3
      case 4           => 4
      case _           => 5
    }
    (Risk(z, ColorMap()), Some(NaturalLink((1 to z).toList)))
  }
}
