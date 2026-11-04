package risk.ui

import gbge.shared.FrontendUniverse
import gbge.ui.ClientGameProps
import gbge.ui.eps.player.ClientState
import gbge.ui.eps.spectator.SpectatorState
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all._
import org.scalajs.dom.html.Div
import risk.shared.{ClientRisk, ClientRiskPlayer, RiskProps}
import risk.shared.abstract0.RiskAction
import uiglue.{Event, EventLoop}
import zio.{UIO, ZIO}

object RiskGameProps
    extends RiskProps
    with ClientGameProps[RiskAction, ClientRisk] {

  override val handleNewFU: (ClientState, FrontendUniverse) => (
      ClientState,
      EventLoop.EventHandler[Event] => UIO[List[Event]]
  ) = (clientState, newFU) => {
    val clientRisk = newFU.game.get.asInstanceOf[ClientRisk]
    val tab2 = clientRisk.innerRisk.flatMap(_.campaign).isDefined
    val newTab = if (tab2) CampaignTab else MapTab
    val newClientState = clientState.offlineState match {
      case currentOfflineRiskState: OfflineRiskState => {
        val newActionMode =
          if (tab2) None else currentOfflineRiskState.actionMode
        clientState.copy(
          frontendUniverse = Some(newFU),
          offlineState = currentOfflineRiskState.copy(
            tab = newTab,
            actionMode = newActionMode
          )
        )
      }
      case _ =>
        clientState.copy(
          frontendUniverse = Some(newFU),
          offlineState = OfflineRiskState(tab = newTab)
        )
    }
    (newClientState, _ => ZIO.succeed(List.empty))
  }

  override val playerDisplayer
      : (ClientState, EventLoop.EventHandler[Event]) => TagOf[Div] =
    Screens.renderForPlayer

  override val spectatorDisplayer
      : (SpectatorState, EventLoop.EventHandler[Event]) => TagOf[Div] =
    (state, _) => {
      Screens.renderForSpectator(
        state.frontendUniverse.get.game.get,
        state.frontendUniverse.get.players
      )
    }

  override val adminDisplayer
      : (ClientState, EventLoop.EventHandler[Event]) => TagOf[Div] = (_, _) => {
    div("admin displayer")
  }

  override val metaExtension
      : (ClientState, EventLoop.EventHandler[Event]) => TagOf[Div] =
    (state, commander) => {
      val risk = state.frontendUniverse.get.game.get.asInstanceOf[ClientRisk]
      val you = state.playerMaybe.get
      val riskPlayer: Option[ClientRiskPlayer] = risk.innerRisk.flatMap(
        _.players.find(player => you.role.contains(player.role))
      )
      if (riskPlayer.isEmpty) {
        div("Seems like you are not in this game...")
      } else {
        div(
          div(
            border := "solid 1px white",
            input(
              `type` := "checkbox",
              checked := riskPlayer.get.rollDefenderDieAutomatically,
              readOnly := true
            ),
            div("Roll defender die automatically."),
            onClick --> Callback {
              commander(ClientToggleRollDefenderDieAutomatically)
            }
          ),
          br,
          div(
            border := "solid 1px white",
            input(
              `type` := "checkbox",
              checked := riskPlayer.get.rollAttackerDieAutomatically,
              readOnly := true
            ),
            div("Roll attacker die automatically."),
            onClick --> Callback {
              commander(ClientToggleRollAttackerDieAutomatically)
            }
          ),
          br,
          div(
            border := "solid 1px white",
            input(
              `type` := "checkbox",
              checked := riskPlayer.get.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent,
              readOnly := true
            ),
            div(
              "Defend with one automatically when only one unit is available."
            ),
            onClick --> Callback {
              commander(
                ClientToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent
              )
            }
          )
        )
      }
    }
}
