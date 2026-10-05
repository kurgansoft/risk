package risk.ui

import gbge.shared.FrontendGame
import gbge.shared.actions.GameAction
import gbge.ui.EntryPoint
import risk.shared.ClientRisk
import zio.json.JsonCodec

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("ep")
object RiskUILauncher extends EntryPoint {
  gbge.shared.RG.gameCodecs = List(ClientRisk.codec.asInstanceOf[JsonCodec[FrontendGame[_ <: GameAction]]])
  gbge.ui.RG.registeredGames = List(RiskGameProps)
}
