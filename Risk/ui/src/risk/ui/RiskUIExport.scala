package risk.ui

import gbge.shared.{FrontendGame, FrontendPlayer, FrontendUniverse}
import gbge.ui.eps.player.ClientState
import gbge.ui.UIExport
import org.scalajs.dom.html.Div
import gbge.client._
import gbge.ui.eps.spectator.SpectatorState
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all._
import risk.shared.{ClientRisk, ClientRiskPlayer}

object RiskUIExport extends UIExport {
  override val playerDisplayer: (ClientState, ClientEventHandler[ClientEvent]) => TagOf[Div] = Screens.renderForPlayer

  override val spectatorDisplayer: (SpectatorState, ClientEventHandler[ClientEvent]) => VdomTagOf[Div] = (state, _) => {
    Screens.renderForSpectator(state.frontendUniverse.get.game.get, state.frontendUniverse.get.players)
  }

  override val handleNewFU: (ClientState, FrontendUniverse)  => (ClientState, ClientResult) = (clientState, newFU) => {
    val clientRisk = newFU.game.get.asInstanceOf[ClientRisk]
    val tab2 = clientRisk.innerRisk.flatMap(_.campaign).isDefined
    val newTab = if (tab2) CampaignTab else MapTab
    clientState.offlineState match {
      case currentOfflineRiskState: OfflineRiskState => {
        val newActionMode = if (tab2) {
          None
        } else {
          currentOfflineRiskState.actionMode
        }
        (clientState.copy(frontendUniverse = Some(newFU), offlineState = currentOfflineRiskState.copy(tab = newTab, actionMode = newActionMode)), OK)
      }
      case _ => (clientState.copy(frontendUniverse = Some(newFU), offlineState = OfflineRiskState(tab = newTab)), OK)
    }
  }
  override val adminDisplayer: (ClientState, ClientEventHandler[ClientEvent]) => TagOf[Div] = (clientState, commander) => {
    div("admin displayer")
  }


  override val metaExtension: (ClientState, ClientEventHandler[ClientEvent]) => VdomTagOf[Div] = (state, commander) => {
    import japgolly.scalajs.react.Callback
    import japgolly.scalajs.react.vdom.all._
    val risk = state.frontendUniverse.get.game.get.asInstanceOf[ClientRisk]
    val you = state.you.get
    val riskPlayer: Option[ClientRiskPlayer] = risk.innerRisk.flatMap(_.players.find(player => you.role.contains(player.role)))
    if (riskPlayer.isEmpty) {
      div("Seems like you are not in this game...")
    } else {
      div(
        div(border:= "solid 1px white",
          input(`type`:= "checkbox", checked:= riskPlayer.get.rollDefenderDieAutomatically, readOnly:= true),
          div("Roll defender die automatically."),
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(ClientToggleRollDefenderDieAutomatically)
          }
        ), br,
        div(border:= "solid 1px white",
          input(`type`:= "checkbox", checked:= riskPlayer.get.rollAttackerDieAutomatically, readOnly:= true),
          div("Roll attacker die automatically."),
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(ClientToggleRollAttackerDieAutomatically)
          }
        ), br,
        div(border:= "solid 1px white",
          input(`type`:= "checkbox", checked:= riskPlayer.get.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent, readOnly:= true),
          div("Defend with one automatically when only one unit is available."),
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(ClientToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent)
          }
        )
      )
    }
  }
}