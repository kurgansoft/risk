package risk.shared

import gbge.shared.{FrontendGame, GameState}
import risk.shared.abstract0.{AbstractRisk, RiskAction, RiskRole}
import zio.blocks.schema.Schema
import zio.blocks.schema.json.{Json, JsonCodec}

case class ClientRisk(
    override val noOfPlayers: Int = 3,
    override val innerRisk: Option[ClientInnerRisk],
    override val activeRoles: Set[RiskRole]
) extends AbstractRisk
    with FrontendGame[RiskRole, RiskAction] {
  override lazy val encode: Json = ClientRisk.codec.encodeToJson(this)
}

object ClientRisk {
  val codec: JsonCodec[ClientRisk] = Schema.derived.jsonCodec
}
