package risk.shared

import gbge.shared.{FrontendGame, GameState}
import risk.shared.abstract0.{AbstractRisk, ColorMap, RiskAction}
import zio.json.ast.Json
import zio.json.JsonCodec
import zio.schema.{DeriveSchema, Schema}

case class ClientRisk(
                       override val noOfPlayers: Int = 3,
                       override val colorMap: ColorMap = ColorMap(),
                       override val state: GameState = GameState.NOT_STARTED,
                       override val innerRisk: Option[ClientInnerRisk]
                     ) extends AbstractRisk with FrontendGame[RiskAction] {
  override lazy val encode: Json = ClientRisk.codec.encoder.toJsonAST(this).getOrElse(???)
}

object ClientRisk {
  implicit val schema: Schema[ClientRisk] = DeriveSchema.gen[ClientRisk]
  implicit val codec: JsonCodec[ClientRisk] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}
