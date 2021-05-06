package risk.ui

import gbge.client.{ClientEvent, ClientEventHandler}
import gbge.shared.{FrontendGame, FrontendPlayer, IN_PROGRESS, NOT_STARTED}
import gbge.ui.eps.player.ClientState
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all.div
import org.scalajs.dom.html.Div
import japgolly.scalajs.react.vdom.all._
import risk.shared.{ClientRisk, ClientRiskPlayer}

import scala.util.Try

object Screens {
  def renderForPlayer(clientState: ClientState, commander: ClientEventHandler[ClientEvent]): TagOf[Div] = {
    assert(clientState.frontendUniverse.isDefined)
    val fu = clientState.frontendUniverse.get
    assert(fu.game.isDefined)
    val risk = fu.game.get.asInstanceOf[ClientRisk]
    val players = fu.players
    assert(clientState.you.isDefined)
    val offlineRiskState: OfflineRiskState = clientState.offlineState.asInstanceOf[OfflineRiskState]

    val theDiv = if (risk.state == NOT_STARTED) {
      div(
        div(display:="flex", flexDirection:="row",
          Option.when(clientState.isAdmin)
          (button(`class`:="btn btn-primary", "-", onClick --> Callback {
            commander.addAnEventToTheEventQueue(ClientDecreaseProposedNumberOfPlayers)
          })),
          span(`class`:="badge badge-info", risk.noOfPlayers, marginLeft:="20px", marginRight:="20px"),
          Option.when(clientState.isAdmin)
          (button(`class`:="btn btn-primary", "+", onClick --> Callback {
            commander.addAnEventToTheEventQueue(ClientIncreaseProposedNumberOfPlayers)
          }))
        ),
        div(display:="flex", flexDirection:="row",
          Directives.playerStatus(clientState.you.get, risk.colorMap),
          Directives.playerStatuses(risk.colorMap, players),
        ),
        Directives.riskRoleChooser(clientState.you.get, risk.colorMap, commander)(players),
        Option.when(clientState.you.get.isAdmin)
        (div(
          hr,hr,hr,
          div(
            if (risk.readyToStart)
            div(margin:="0 auto", width:="300px",
              button(`class`:="btn btn-primary", "START", onClick --> Callback {
                commander.addAnEventToTheEventQueue(ClientInit)
              })
            ) else
            div("Cannot start the game just yet.")
          )
        ))
      )
    } else {
      if (risk.innerRisk.isDefined) {
        val innerRisk = risk.innerRisk.get
        val rp0: Option[ClientRiskPlayer] = clientState.you.map(_.role)
          .flatMap(role000 => innerRisk.players.find(x => role000.contains(x.role)))
        if (rp0.isEmpty) {
          div("You are not in this game...")
        } else {
          val rp = rp0.get
          val distinct = offlineRiskState.tab match {
            case MapTab => {
              div(display := "flex", flexDirection := "row", justifyContent := "space-between",
                div(
                  Directives.hungary(innerRisk.territories, risk.colorMap, innerRisk.arrow, Some(commander)),
                  div(display := "flex", flexDirection := "row",
                    Directives.infoScreen(clientState.you.get, innerRisk, risk.colorMap)(players),
                    div(width := "50px"),
                    Directives.playersScoreDisplayer(risk.colorMap, innerRisk.players, innerRisk.currentPlayer)(players),
                    div(width := "50px"),
                    div(
                      div("Phase: " + innerRisk.phase),
                      div("Round: " + innerRisk.round)
                    )
                  )(marginLeft := "20px"), br,
                  Directives.hint(clientState.you.get, innerRisk, risk.colorMap),
                  Directives.actionSelector(offlineRiskState.actionMode, clientState.you.get, innerRisk, commander)(position := "absolute", top := "35px", left := "20px")
                ),
                Directives.tabSwitcher(commander, 1, innerRisk.campaign.isDefined, rp.inventorySize)(marginRight := "25px")
              )
            }
            case CampaignTab => {
              div(position := "relative", height := "100%",
                div(display := "flex", flexDirection := "row", justifyContent := "space-between", position := "relative", height := "100%",
                  div(width := "600px", VdomStyle("order") := 2, width := "50%", position := "relative",
                    Directives.campaignComponent(innerRisk.campaign.get, innerRisk.territories)(players)
                  ),
                  div(VdomStyle("order") := 1, width := "25%",
                    Directives.infoScreen2(clientState.you, innerRisk.campaign.get, innerRisk.territories),
                    Directives.battleButtons(commander, clientState.you.flatMap(_.role), innerRisk)
                  ),
                  div(VdomStyle("order") := 3, width := "25%", display := "flex", flexDirection := "row", alignItems := "center", position := "relative",
                    Directives.miniMap0(innerRisk.territories, risk.colorMap, innerRisk.arrow.get)()(position := "relative", right := "25px")
                  )
                ),
                Directives.tabSwitcher(commander, 2, inventorySize = rp.inventorySize)
                (marginRight := "25px", position := "absolute", right := "0px", top := "0px")
              )
            }
            case InventoryTab => {
              val innerRisk = risk.innerRisk.get
              div(display := "flex", flexDirection := "row", justifyContent := "space-between",
                div(
                  if (rp.inventorySize == 0)
                    h1("Your inventory is empty.")
                  else {
                    div(
                      h1("These are the items in your inventory:"),
                      ul(
                        rp.inventory.getOrElse(List.empty).map(item => ul(item)).toTagMod
                      )
                    )
                  }
                ),
                Directives.tabSwitcher(commander, 3, innerRisk.campaign.isDefined, rp.inventorySize)(marginRight := "25px")
              )
            }
          }
          div(position := "relative", height := "100%",
            distinct(height := "100%"),
            Directives.playerStatus2(clientState.you.get, risk.colorMap)(position := "absolute", bottom := "5px", right := "5px")
          )
        }
      } else
        div( "???")
    }
    theDiv(color:="yellow", height:="100%")
  }

  def renderForSpectator(fg: FrontendGame[_], players: List[FrontendPlayer]): TagOf[Div] = {
    assert(fg.isInstanceOf[ClientRisk])
    val clientRisk = fg.asInstanceOf[ClientRisk]
    val innerRisk = clientRisk.innerRisk
    val tsPcm = Try((innerRisk.get.territories, clientRisk.colorMap))
    clientRisk.state match {
      case IN_PROGRESS =>
        div(color:="yellow",
          Option.when(tsPcm.isSuccess)
          (
            div(display:="flex", flexDirection:="row",
              div(
                Directives.hungary(tsPcm.get._1, tsPcm.get._2, innerRisk.flatMap(_.arrow)),
                Option.when(innerRisk.isDefined)
                (
                  div(display:="flex", flexDirection:="row",
                    div(
                      div("Phase: " + innerRisk.get.phase),
                      div("Round: " + innerRisk.get.round)
                    ),
                    div(width:="50px"),
                    Directives.playersScoreDisplayer(clientRisk.colorMap, innerRisk.get.players, innerRisk.get.currentPlayer)(players),
                    div(width:="50px")
                  )
                )
              ),
              Option.when(innerRisk.get.campaign.isDefined)
              (Directives.campaignComponent(innerRisk.get.campaign.get, innerRisk.get.territories)(players))
            )
          )
        )
      case NOT_STARTED =>
        div(position:="absolute", top:="0px", bottom:="0px", left:="0px", right:="0px",
          div(height:="1024px", width:="1418px",
            position:="absolute", top:="50%", left:="50%", margin:="-512px 0 0 -709px",
            Directives.playerStatuses(clientRisk.colorMap, players),
          )
        )
    }

  }
}
