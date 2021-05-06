package base

import gbge.ui.EntryPoint
import risk.shared.ClientRisk
import risk.ui.RiskUIExport

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("ep")
object RiskUILauncher extends EntryPoint {
   gbge.shared.RG.registeredGames = List(ClientRisk)
   gbge.ui.RG.registeredGames = List(RiskUIExport)
}