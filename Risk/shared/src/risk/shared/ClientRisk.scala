package risk.shared

import gbge.shared.{DecodeCapable, FrontendGame, GameState, NOT_STARTED}
import risk.shared.abstract0.{AbstractRisk, ColorMap, RiskAction}
import upickle.default.{macroRW, ReadWriter => RW}

case class ClientRisk(
                       override val noOfPlayers: Int = 3,
                       override val colorMap: ColorMap = ColorMap(),
                       override val state: GameState = NOT_STARTED,
                       override val innerRisk: Option[ClientInnerRisk]
                     ) extends AbstractRisk with FrontendGame[RiskAction] {
  override def serialize(): String = upickle.default.write(this)

  override def decodeAction(payload: String): RiskAction = {
    upickle.default.read[RiskAction](payload)
  }
}

object ClientRisk extends DecodeCapable {
  implicit def rw: RW[ClientRisk] = macroRW

  override def decode(encodedForm: String): ClientRisk = {
    upickle.default.read[ClientRisk](encodedForm)
  }

  override val name: String = "Risk"
}