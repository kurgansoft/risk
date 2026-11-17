package risk.ui

import gbge.shared.{FrontendGame, FrontendPlayer, GameState}
import gbge.ui.eps.player.ClientState
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all.div
import org.scalajs.dom.html.Div
import japgolly.scalajs.react.vdom.all.*
import risk.shared.abstract0.RiskRole
import risk.shared.{ClientRisk, ClientRiskPlayerInfo}
import uiglue.{Event, EventLoop}

import scala.util.Try

object Screens {
  def renderForPlayer(
      clientState: ClientState,
      commander: EventLoop.EventHandler[Event]
  ): TagOf[Div] = {
    assert(clientState.frontendUniverse.isDefined)
    val fu = clientState.frontendUniverse.get
    assert(fu.game.isDefined)
    val risk = fu.game.get.asInstanceOf[ClientRisk]
    val players = fu.players
    assert(clientState.playerMaybe.isDefined)
    val offlineRiskState: OfflineRiskState =
      clientState.offlineState.asInstanceOf[OfflineRiskState]

    val theDiv = if (risk.innerRisk.isEmpty) {
      Directives.renderLobby(risk, players, clientState.playerMaybe)(commander)
//      val playersWithRoles = players.filter(_.role.isDefined)
//      val playersWithoutRoles = players.filterNot(_.role.isDefined)
//      div(
//        div(
//          display := "flex",
//          flexDirection := "column",
//          VdomStyle("gap") := "20px",
//          padding := "24px",
//          maxWidth := "1100px",
//          margin := "0 auto",
//          color := "#243247",
//          div(
//            display := "flex",
//            flexWrap := "wrap",
//            justifyContent := "space-between",
//            alignItems := "center",
//            VdomStyle("gap") := "16px",
//            padding := "20px 24px",
//            backgroundColor := "white",
//            borderRadius := "10px",
//            boxShadow := "0 2px 8px rgba(0, 0, 0, 0.12)",
//            div(
//              h2("Game lobby")(margin := "0"),
//              div("Choose a role and color, then wait for the game to start.")
//            ),
//            div(
//              display := "flex",
//              flexDirection := "row",
//              alignItems := "center",
//              VdomStyle("gap") := "12px",
//              span("Players", fontWeight.bold),
//              Option.when(clientState.isAdmin)(
//                button(
//                  `type` := "button",
//                  `class` := "btn btn-primary",
//                  "-",
//                  aria.label := "Decrease number of players",
//                  onClick --> Callback {
//                    commander(ClientDecreaseProposedNumberOfPlayers)
//                  }
//                )
//              ),
//              span(
//                `class` := "badge badge-info",
//                risk.noOfPlayers,
//                fontSize := "18px",
//                padding := "8px 14px"
//              ),
//              Option.when(clientState.isAdmin)(
//                button(
//                  `type` := "button",
//                  `class` := "btn btn-primary",
//                  "+",
//                  aria.label := "Increase number of players",
//                  onClick --> Callback {
//                    commander(ClientIncreaseProposedNumberOfPlayers)
//                  }
//                )
//              )
//            )
//          ),
//          div(
//            display := "flex",
//            flexWrap := "wrap",
//            alignItems := "stretch",
//            VdomStyle("gap") := "20px",
//            div(
//              flex := "1 1 260px",
//              padding := "18px",
//              backgroundColor := "#243247",
//              color := "white",
//              borderRadius := "10px",
//              h3("Your status")(marginTop := "0"),
//              Directives.playerStatus(
//                clientState.playerMaybe.get
//              )
//            ),
//            div(
//              flex := "2 1 360px",
//              padding := "18px",
//              backgroundColor := "white",
//              borderRadius := "10px",
//              boxShadow := "0 2px 8px rgba(0, 0, 0, 0.12)",
//              h3("Players in the lobby")(marginTop := "0"),
//              div(
//                display := "flex",
//                flexWrap := "wrap",
//                VdomStyle("gap") := "8px",
//                (playersWithRoles.map { player =>
//                  val role = player.role.get
//                  val playerColor = RiskRole0
//                    .getPlayerFromRoleId(role)
//                    .color
//                    .toString
//                    .toLowerCase
//                  div(
//                    padding := "8px 12px",
//                    backgroundColor := "#eef2f7",
//                    borderRadius := "6px",
//                    player.name + " - " + role + " [" + playerColor + "]"
//                  )
//                } ++ playersWithoutRoles.map { player =>
//                  div(
//                    padding := "8px 12px",
//                    backgroundColor := "#eef2f7",
//                    borderRadius := "6px",
//                    player.name
//                  )
//                }).toTagMod
//              )
//            )
//          ),
//          div(
//            display := "flex",
//            flexWrap := "wrap",
//            alignItems := "flex-start",
//            VdomStyle("gap") := "20px",
//            div(
//              flex := "1 1 360px",
//              padding := "18px",
//              backgroundColor := "white",
//              borderRadius := "10px",
//              boxShadow := "0 2px 8px rgba(0, 0, 0, 0.12)",
//              h3("Choose your color")(marginTop := "0"),
//              Directives.riskRoleChooser(
//                clientState.playerMaybe.get,
//                commander
//              )(using players)
//            ),
//            Option.when(clientState.isAdmin)(
//              div(
//                flex := "1 1 260px",
//                padding := "18px",
//                backgroundColor := "white",
//                borderRadius := "10px",
//                boxShadow := "0 2px 8px rgba(0, 0, 0, 0.12)",
//                h3("Start the game")(marginTop := "0"),
//                if (risk.readyToStart)
//                  button(
//                    `type` := "button",
//                    `class` := "btn btn-primary",
//                    "Start game",
//                    onClick --> Callback {
//                      commander(ClientInit)
//                    }
//                  )
//                else
//                  div("The game cannot be started yet.")
//              )
//            )
//          )
//        )
//      )
    } else {
      val innerRisk = risk.innerRisk.get
      val rp0: Option[ClientRiskPlayerInfo] = clientState.playerMaybe
        .map(_.role)
        .flatMap(role000 =>
          innerRisk.players.find(x => role000.contains(x._1.roleId))
        )
        .map(_._2)
      if (rp0.isEmpty) {
        div("You are not in this game...")
      } else {
        val rp = rp0.get
        val distinct = offlineRiskState.tab match {
          case MapTab =>
            div(
              display := "flex",
              flexDirection := "row",
              justifyContent := "space-between",
              div(
                Directives.hungary(
                  innerRisk.territories,
                  innerRisk.arrow,
                  Some(commander)
                ),
                div(
                  display := "flex",
                  flexDirection := "row",
                  Directives.infoScreen(
                    clientState.playerMaybe.get,
                    innerRisk
                  )(using players),
                  div(width := "50px"),
                  Directives.playersScoreDisplayer(
                    innerRisk.players,
                    innerRisk.roleOfTheCurrentPlayer.roleId
                  )(using players),
                  div(width := "50px"),
                  div(
                    div("Phase: " + innerRisk.phase),
                    div("Round: " + innerRisk.round)
                  )
                )(marginLeft := "20px"),
                br,
                Directives.hint(
                  clientState.playerMaybe.get,
                  innerRisk
                ),
                Directives.actionSelector(
                  offlineRiskState.actionMode,
                  clientState.playerMaybe.get,
                  innerRisk,
                  commander
                )(position := "absolute", top := "35px", left := "20px")
              ),
              Directives.tabSwitcher(
                commander,
                1,
                innerRisk.campaign.isDefined,
                rp.inventorySize
              )(marginRight := "25px")
            )
          case CampaignTab =>
            div(
              position := "relative",
              height := "100%",
              div(
                display := "flex",
                flexDirection := "row",
                justifyContent := "space-between",
                position := "relative",
                height := "100%",
                div(
                  width := "600px",
                  VdomStyle("order") := 2,
                  width := "50%",
                  position := "relative",
                  Directives.campaignComponent(
                    innerRisk.campaign.get,
                    innerRisk.territories
                  )(using players)
                ),
                div(
                  VdomStyle("order") := 1,
                  width := "25%",
                  Directives.infoScreen2(
                    clientState.playerMaybe,
                    innerRisk.campaign.get,
                    innerRisk.territories
                  ),
                  Directives.battleButtons(
                    commander,
                    clientState.playerMaybe.flatMap(_.role),
                    innerRisk
                  )
                ),
                div(
                  VdomStyle("order") := 3,
                  width := "25%",
                  display := "flex",
                  flexDirection := "row",
                  alignItems := "center",
                  position := "relative",
                  Directives.miniMap0(
                    innerRisk.territories,
                    innerRisk.arrow.get
                  )()(position := "relative", right := "25px")
                )
              ),
              Directives
                .tabSwitcher(commander, 2, inventorySize = rp.inventorySize)(
                  marginRight := "25px",
                  position := "absolute",
                  right := "0px",
                  top := "0px"
                )
            )
          case InventoryTab =>
            val innerRisk = risk.innerRisk.get
            div(
              display := "flex",
              flexDirection := "row",
              justifyContent := "space-between",
              div(
                if (rp.inventorySize == 0)
                  h1("Your inventory is empty.")
                else {
                  div(
                    h1("These are the items in your inventory:"),
                    ul(
                      rp.inventory
                        .getOrElse(List.empty)
                        .map(item => ul(item))
                        .toTagMod
                    )
                  )
                }
              ),
              Directives.tabSwitcher(
                commander,
                3,
                innerRisk.campaign.isDefined,
                rp.inventorySize
              )(marginRight := "25px")
            )
        }
        div(
          position := "relative",
          height := "100%",
          distinct(height := "100%"),
          Directives.playerStatus2(
            clientState.playerMaybe.get
          )(position := "absolute", bottom := "5px", right := "5px")
        )
      }
    }
    theDiv(color := "yellow", height := "100%")
  }

  def renderForSpectator(
      fg: FrontendGame[?, ?],
      players: List[FrontendPlayer]
  ): TagOf[Div] = {
    assert(fg.isInstanceOf[ClientRisk])
    val clientRisk = fg.asInstanceOf[ClientRisk]
    val innerRisk = clientRisk.innerRisk
    clientRisk.innerRisk match {
      case Some(innerRisk) =>
        div(
          color := "yellow",
          div(
            display := "flex",
            flexDirection := "row",
            div(
              Directives.hungary(
                innerRisk.territories,
                innerRisk.arrow
              ),
              div(
                display := "flex",
                flexDirection := "row",
                div(
                  div("Phase: " + innerRisk.phase),
                  div("Round: " + innerRisk.round)
                ),
                div(width := "50px"),
                Directives.playersScoreDisplayer(
                  innerRisk.players,
                  innerRisk.roleOfTheCurrentPlayer.roleId
                )(using players),
                div(width := "50px")
              )
            ),
            Option.when(innerRisk.campaign.isDefined)(
              Directives.campaignComponent(
                innerRisk.campaign.get,
                innerRisk.territories
              )(using players)
            )
          )
        )
      case None =>
        div(
          display := "flex",
          flexDirection := "column",
          VdomStyle("gap") := "24px",
          width := "100%",
          padding := "24px",
          color := "yellow",
          div(
            s"${clientRisk.noOfPlayers} players are supported.",
            fontSize := "36px",
            fontWeight.bold,
            textAlign := "center"
          ),
          div(
            s"Game will start once ${clientRisk.noOfPlayers} roles are taken and the admin (${players.find(_.isAdmin).map(_.name).getOrElse("???")}) starts the game.",
            fontSize := "36px",
            fontWeight.bold,
            textAlign := "center"
          ),
          Directives.lobbyPlayerLists(players)
        )
    }

  }
}
