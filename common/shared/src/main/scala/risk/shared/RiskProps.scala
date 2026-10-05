package risk.shared

import gbge.shared.GameProps
import risk.shared.abstract0.RiskAction
import zio.json.JsonCodec

trait RiskProps extends GameProps[RiskAction, ClientRisk] {
  override val name: String = "Risk"
  override val urlFragment: String = "risk"
  override implicit val actionCodec: JsonCodec[RiskAction] = RiskAction.codec
  override implicit val gameCodec: JsonCodec[ClientRisk] = ClientRisk.codec
}
