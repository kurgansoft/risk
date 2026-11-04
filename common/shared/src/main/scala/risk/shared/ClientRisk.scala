package risk.shared

import gbge.shared.{FrontendGame, GameState}
import risk.shared.abstract0.{AbstractRisk, ColorMap, RiskAction}
import zio.blocks.schema.Schema
import zio.blocks.schema.json.{Json, JsonCodec}

case class ClientRisk(
    override val noOfPlayers: Int = 3,
    override val colorMap: ColorMap = ColorMap(),
    override val state: GameState = GameState.NOT_STARTED,
    override val innerRisk: Option[ClientInnerRisk]
) extends AbstractRisk
    with FrontendGame[RiskAction] {
  override lazy val encode: Json = ClientRisk.codec.encodeToJson(this)
}

object ClientRisk {
  val codec: JsonCodec[ClientRisk] = Schema.derived.jsonCodec
}
