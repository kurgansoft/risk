package risk.ui

import gbge.client.{ClientEvent, ClientEventHandler, DispatchActionWithToken}
import gbge.shared.FrontendPlayer
import japgolly.scalajs.react.{Callback, ReactEventFromInput}
import japgolly.scalajs.react.vdom.TagOf
import org.scalajs.dom.html.{Button, Div, Input, TableSection}
import japgolly.scalajs.react.vdom.all._
import risk.shared.{ClientBattle, ClientCampaign, ClientInnerRisk, ClientRiskPlayer}
import risk.shared.abstract0.{ABANDONED, ATTACKER_LOST, AddingUnits, BattleAndManeuver, ColorMap, DEFENDER_LOST, DeclareNoOfTroopsPhase, DeployingUnits, DiceThrowingPhase, DropColor, PickColor, RUNNING, ResultPhase, RiskColor, TerritoryOccupationInfo}

object Directives {

  def integerToDieString(integer: Int): String = integer match {
    case 1 => "⚀"
    case 2 => "⚁"
    case 3 => "⚂"
    case 4 => "⚃"
    case 5 => "⚄"
    case 6 => "⚅"
    case _ => "-"
  }

  def selectX(string: String): String = "<<< " + string + " >>>"

  def playerStatus2(you: FrontendPlayer, colorMap: ColorMap): TagOf[Div] = {
    val yourColor: Option[RiskColor] = colorMap.colorOfRole(you.role.getOrElse(-1))
    val square: String = "■"
    div(fontSize:="xx-large", you.name, Option.when(yourColor.isDefined)
      (span(span(" - ")(span(color:=yourColor.get.toString, square))))
    )
  }

  def playerStatus(you: FrontendPlayer, colorMap: ColorMap): TagOf[Div] = {
    val yourColor: Option[RiskColor] = colorMap.colorOfRole(you.role.getOrElse(-1))
    div(border:="solid 1px white",
      div("You are: " + you.name),
      if (you.role.isDefined)
        div("Your role: " +you.role.get)
      else
        div("You have no role in this game."),
      if (yourColor.isDefined)
        div(s"Your color is: ${yourColor.get}")
      else
        div("Choose a color!"),
      div("Admin: " + you.isAdmin),
    )
  }

  def playerStatuses(colorMap: ColorMap, players: List[FrontendPlayer]): TagOf[Div] = {
    val playersWithRoles = players.filter(_.role.isDefined)
    val playersWithOutRoles = players.filterNot(_.role.isDefined)
    div(color:="yellow",
      playersWithRoles.map(player => div(player.name + " - " + player.role.get + " [" +
        colorMap.colorOfRole(player.role.get).map(_.toString).getOrElse("NO COLOR")
        + "]")).toTagMod,
      playersWithOutRoles.map(player => div(player.name)).toTagMod,
    )
  }

  def riskRoleChooser(you: FrontendPlayer, colorMap: ColorMap, commander: ClientEventHandler[ClientEvent])(implicit players: List[FrontendPlayer]):  TagOf[Div] = {
    val colorsTaken = colorMap.colorMap.keys.toList
    val availableColors: List[RiskColor] = RiskColor.enumerateColors.filterNot(colorsTaken.contains(_))
    val yourColor: Option[RiskColor] = colorMap.colorOfRole(you.role.getOrElse(-1))
    div(display:="flex", flexDirection:="row", alignItems:="flex-start",
      div(width:="100px",
        availableColors.map(color => {
          button(`type`:="button", width:="100px", `class`:="btn btn-primary", color.toString, onClick --> Callback {
            commander.addAnEventToTheEventQueue(DispatchActionWithToken(PickColor(you.role.get, color)))
          })
        }).toTagMod
      ),
      Option.when(yourColor.isDefined)
      (button(`type`:="button", `class`:="btn btn-primary", "DROP YOUR COLOR", onClick --> Callback {
        commander.addAnEventToTheEventQueue(DispatchActionWithToken(DropColor(you.role.get)))
      }))
    )
  }

  def hint(you: FrontendPlayer, innerRisk: ClientInnerRisk, colorMap: ColorMap): TagOf[Div] = {
    val message: String = innerRisk.phase match {
      case AddingUnits => {
        if(you.role.contains(innerRisk.currentPlayer)) {
          val color = colorMap.colorOfRole(you.role.get)
          if (color.isDefined)
            "Click on a " + color.get.toString.toLowerCase + " tile to place a unit there."
          else
            "You are not in this game."
        } else {
          "Wait for your turn."
        }
      }
      case _ => "NO HINT"
    }
    div("HINT: " + message)
  }

  def actionSelector(actionMode: Option[OfflineActionMode], you: FrontendPlayer, innerRisk: ClientInnerRisk, commander: ClientEventHandler[OfflineRiskEvent]): TagOf[Div] = {
    innerRisk.phase match {
      case AddingUnits => div()
      case DeployingUnits => {
        if (you.role.contains(innerRisk.currentPlayer)) {
          val allTroopsAreDeployed = innerRisk.getCurrentPlayer.troops == 0
          val duText = if (actionMode.contains(DeployUnitMode)) selectX("DEPLOY UNIT") else "DEPLOY UNIT"
          val ruText = if (actionMode.contains(RetractUnitMode)) selectX("RETRACT UNIT") else "RETRACT UNIT"
          div(border := "solid 1px red", display := "flex", flexDirection := "column",
            button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", duText , onClick --> Callback {
              commander.addAnEventToTheEventQueue(SelectOfflineActionMode(DeployUnitMode))
            }),
            button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled:=false, ruText, onClick --> Callback {
              commander.addAnEventToTheEventQueue(SelectOfflineActionMode(RetractUnitMode))
            }),
            button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled:= !allTroopsAreDeployed, "COMMIT DEPLOYMENT", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientCommit)
            })
          )
        } else div()
      }
      case BattleAndManeuver => {
        if (innerRisk.campaign.map(_.campaignStatus).contains(RUNNING)) {
          div("A campaign is in progress.")
        } else {
          if (you.role.contains(innerRisk.currentPlayer)) {
            actionMode match {
              case None => {
                val buttons = List(
                  button(`type` := "button", `class` := "btn btn-primary", "LAUNCH CAMPAIGN", onClick --> Callback {
                    commander.addAnEventToTheEventQueue(SelectOfflineActionMode(AttackMode()))
                  }),
                  button(`type` := "button", `class` := "btn btn-primary", "MANEUVER", onClick --> Callback {
                    commander.addAnEventToTheEventQueue(SelectOfflineActionMode(ManeuverMode()))
                  }),
                  button(`type` := "button", `class` := "btn btn-primary", "SKIP", onClick --> Callback {
                    commander.addAnEventToTheEventQueue(ClientSkip)
                  })
                )
                buttonStack(buttons)
              }
              case Some(AttackMode(source, destination)) => {
                if (source.isEmpty) {
                  div(border := "solid 1px red", display := "flex", flexDirection := "column",
                    div("Click on the territory where you want to attack from!"),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled := false, "CANCEL", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(CancelOfflineActionMode)
                    })
                  )
                } else if (source.isDefined && destination.isEmpty) {
                  div(border := "solid 1px red", display := "flex", flexDirection := "column",
                    div(s"Attacking from ${source.get}"),
                    div(s"Click on the territory that you want to attack from ${source.get}!"),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled := false, "CANCEL", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(CancelOfflineActionMode)
                    })
                  )
                } else if (source.isDefined && destination.isDefined) {
                  div(border := "solid 1px red", display := "flex", flexDirection := "column",
                    div(s"Attacking ${source.get} -> ${destination.get}"),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled := !(source.isDefined && destination.isDefined), "LAUNCH!!!", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(ClientLaunch)
                    }),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", "CANCEL", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(CancelOfflineActionMode)
                    })
                  )
                } else div()
              }
              case Some(ManeuverMode(from, to, noOfTroops)) => {
                if (from.isEmpty) {
                  div(border := "solid 1px red", display := "flex", flexDirection := "column",
                    div("Click on the territory where you want to maneuver troops from!"),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", "CANCEL", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(CancelOfflineActionMode)
                    })
                  )
                } else if (from.isDefined && to.isEmpty) {
                  div(border := "solid 1px red", display := "flex", flexDirection := "column",
                    div(s"Click on the territory that you want to maneuver troops from ${from.get}!"),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", "CANCEL", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(CancelOfflineActionMode)
                    })
                  )
                } else if (from.isDefined && to.isDefined) {
                  val maxNoOfTroopsToManeuver = innerRisk.territories(from.get).noOfUnits - 1
                  def cb(e: ReactEventFromInput): Callback = Callback {
                    val asInt = Integer.parseInt(e.target.value)
                    commander.addAnEventToTheEventQueue(SelectOfflineActionMode(ManeuverMode(from, to, asInt)))
                  }
                  div(border := "solid 1px red", display := "flex", flexDirection := "column",
                    div(s"Maneuvering troops from ${from.get} to ${to.get}"),
                    div("Specify the number of troops you want to maneuver!"),
                    input(`type` := "number", min := 1, max := maxNoOfTroopsToManeuver, value := noOfTroops, onChange ==> cb),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled := !(from.isDefined && to.isDefined), "MANEUVER", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(ClientManeuverTroops(from.get, to.get, noOfTroops))
                    }),
                    button(`type` := "button", `class` := "btn btn-primary", marginBottom := "10px", disabled := false, "CANCEL", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(CancelOfflineActionMode)
                    })
                  )
                } else div()
              }
              case _ => div("some inconsistency; mode is " + actionMode.get)
            }
          } else div()
        }
      }
      case _ => div()
    }
  }

  def infoScreen(you: FrontendPlayer, innerRisk: ClientInnerRisk, colorMap: ColorMap)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val yourColor: Option[RiskColor] = you.role.flatMap(colorMap.colorOfRole)

    def x1(): TagOf[Div] = {
      if (yourColor.isDefined)
        div("Your color is: " + yourColor.get)
      else
        div("You don't have any color in this game.")
    }

    def x2(): TagOf[Div] = {
      val cp: String = players.find(_.role.contains(innerRisk.currentPlayer)).map(_.name).getOrElse("___MISSING_PLAYER___")
      if(you.role.contains(innerRisk.currentPlayer))
        div("This is your turn.")
      else
        div(s"This is the turn of $cp.")
    }
    div(
      div("You are: " + you.name),
      x1(),
      x2()
    )
  }

  def infoScreen2(you: Option[FrontendPlayer], campaign: ClientCampaign, territories: List[TerritoryOccupationInfo]): TagOf[Div] = {
    val defendersRole = territories(campaign.destination).roleId
    val attackersRole = territories(campaign.source).roleId
    val yourRole: Option[Int] = you.flatMap(_.role)

    val text = {
      if (yourRole.contains(attackersRole))
        "You are the attacker."
      else if (yourRole.contains(defendersRole))
        "You are the defender."
      else
        "You are not involved in this campaign"
    }
    div(text)
  }

  def battleButtons(commander: ClientEventHandler[ClientEvent], yourRole: Option[Int], innerRisk: ClientInnerRisk): TagOf[Div] = {
    assert(innerRisk.campaign.isDefined)
    val campaign = innerRisk.campaign.get
    if (yourRole.contains(innerRisk.defendersRole)) {
      val troopsAtDestination = innerRisk.territories(campaign.destination).noOfUnits
      campaign.campaignStatus match {
        case DEFENDER_LOST => div("It seems that all your forces have been eradicated...")
        case ATTACKER_LOST => div("You have protected this area!")
        case ABANDONED => div("Your opponent has cowardly retreated...")
        case RUNNING => {
          val buttons: List[TagOf[Button]] = List(
            Option.when(troopsAtDestination >= 2)
            (button(`type`:="button", `class`:="btn btn-primary", "DEFEND with 2", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientDefend(2))
            })),
            Some(button(`type`:="button", `class`:="btn btn-primary", "DEFEND with 1", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientDefend(1))
            }))
          ).flatten
          if (campaign.battle.noOfAttackers == 0) {
            div("Waiting for the attacker to declare the number of attackers")
          } else if (campaign.battle.noOfDefenders == 0) {
            div(display:="flex", justifyContent:="space-between",
              buttonStack(buttons)
            )
          } else if (campaign.battle.defenderDie.isEmpty) {
            if (campaign.battle.defenderDieRolling)
              div("Alea iacta est...")
            else
              div(
                button(`type` := "button", `class` := "btn btn-primary", "ROLL " + campaign.battle.noOfDefenders + "D6", onClick --> Callback {
                  commander.addAnEventToTheEventQueue(ClientRollDiceForDefender)
                })
              )
          } else if (campaign.battle.attackerDie.isEmpty) {
            div("Waiting for the attacker to roll the die.")
          } else {
            div("This battle is over. Waiting for the attacker to attack again or retreat.")
          }
        }
      }
    } else if (yourRole.contains(innerRisk.attackersRole)) {
      val troopsAtSource = innerRisk.territories(campaign.source).noOfUnits
      campaign.campaignStatus match {
        case DEFENDER_LOST => {
          val minNoOfTroopsToMoveIn: Int = campaign.battle.remainingAttackingTroops
          val maxNoOfTroopsToMoveIn: Int = innerRisk.territories(campaign.source).noOfUnits-1

          def cb(e: ReactEventFromInput): Callback = Callback {
            val asString = e.target.parentElement.childNodes(2).asInstanceOf[Input].value
            val asInt = Integer.parseInt(asString)
            commander.addAnEventToTheEventQueue(ClientMoveInToConqueredTerritory(asInt))
          }
          val number = if (minNoOfTroopsToMoveIn == maxNoOfTroopsToMoveIn) minNoOfTroopsToMoveIn.toString
            else s"[$minNoOfTroopsToMoveIn - $maxNoOfTroopsToMoveIn]"
          val numberText = if (number == "1" ) "1 unit." else number + " units."
          div(
            div(s"Victory!"),
            div(s"You can march in to the conquered territory with $numberText"),
            input(`type`:="number", min:=minNoOfTroopsToMoveIn, max:=maxNoOfTroopsToMoveIn, defaultValue:= minNoOfTroopsToMoveIn),
            button(`type`:= "button", `class`:= "btn btn-primary", "MOVE IN", onClick ==> cb)
          )
        }
        case ATTACKER_LOST => div("Bad luck...")
        case ABANDONED => div("You have fled the scene with your tail between your legs.")
        case RUNNING => {
          val buttons: List[TagOf[Button]] = List(
            Option.when(troopsAtSource > 3)
            (button(`type`:= "button", `class`:= "btn btn-primary", "ATTACK with 3", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientAttack(3))
            })),
            Option.when(troopsAtSource > 2)
            (button(`type`:= "button", `class`:= "btn btn-primary", "ATTACK with 2", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientAttack(2))
            })),
            Some(button(`type`:= "button", `class`:= "btn btn-primary", "ATTACK with 1", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientAttack(1))
            })),
            Option.when(campaign.battle.noOfAttackers > 0)
            (button(`type`:="button", `class`:="btn btn-primary", "Abandon campaign", onClick --> Callback {
              commander.addAnEventToTheEventQueue(ClientAbandonCurrentCampaign)
            }))
          ).flatten
          if (campaign.battle.noOfAttackers == 0) {
            div(display:="flex", justifyContent:="space-between",
              buttonStack(buttons)
            )
          } else if (campaign.battle.noOfDefenders == 0 ) {
            div("Wating for the defender")
          } else if (campaign.battle.attackerDie.isEmpty) {
            if (campaign.battle.attackerDieRolling)
              div("Alea iacta est...")
            else
              div(
                button(`type`:="button", `class`:="btn btn-primary", "ROLL " + campaign.battle.noOfAttackers + "D6", onClick --> Callback {
                  commander.addAnEventToTheEventQueue(ClientRollDiceForAttacker)
                })
              )
          } else if (campaign.battle.defenderDie.isEmpty) {
            div("Waiting for defender to roll the die.")
          } else {
            div(display:="flex", justifyContent:="space-between",
              buttonStack(buttons)
            )
          }
        }
      }
    } else {
      div()
    }
  }

  def miniMap0(territories: List[TerritoryOccupationInfo], colorMap: ColorMap, arrow: (Int, Int)): TagOf[Div] = {
    div(VdomStyle("userSelect"):="none",
      MapGenerator.miniMap(territories, colorMap, arrow)
    )
  }

  def hungary(territories: List[TerritoryOccupationInfo], colorMap: ColorMap, arrow: Option[(Int, Int)] = None, commander: Option[ClientEventHandler[TerritoryClicked]] = None): TagOf[Div] = {
    assert(territories.size == 20)
    div(VdomStyle("userSelect"):="none",
      MapGenerator.mapOfHungary(territories, colorMap, arrow, commander)
    )
  }

  def playersScoreDisplayer(colorMap: ColorMap, riskPlayers: List[ClientRiskPlayer], currentPlayer: Int)(implicit players: List[FrontendPlayer]):  TagOf[Div] = {
    val q = riskPlayers.sortBy(_.role).map(rp => {
      val playerName = players.find(_.role.contains(rp.role)).map(_.name).getOrElse("###MISSING_PLAYER###")
      val color = colorMap.colorOfRole(rp.role)
      (rp, playerName, color)
    })
    div(borderLeft:="3px solid yellow",
      (for (rp <- q) yield {
        val td = if (rp._1.role == currentPlayer) "underline" else "none"
        val t = if (rp._1.eliminated) " [ELIMINATED]" else rp._1.troops
        val inventory = rp._1.inventorySize match {
          case 1 => " [1 item]"
          case x if x > 1 => " [" + x + " items]"
          case _ => ""
        }
        div(textDecoration:= td, rp._2 + " (" + rp._3.get + ") - " + t + inventory)
      }).toTagMod
    )
  }

  def campaignComponent(campaign: ClientCampaign, territories: List[TerritoryOccupationInfo])(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    div(margin:="auto", width:="540px", display:="flex", flexDirection:="column", height:="100%",
      Directives.generalCampaignInfo(campaign, territories)(players),br,br,
      div("CAMPAIGN STATUS: " + campaign.campaignStatus, textAlign:="center", border:="solid 2px red"),br,br,
      Directives.battleComponent(campaign.battleNumber, campaign.battle)
    )
  }

  def generalCampaignInfo(campaign: ClientCampaign, territories: List[TerritoryOccupationInfo])(implicit players: List[FrontendPlayer]): TagOf[Div] = {

    val prefix = "games/risk/coatOfArms/"

    val attackersRoleId = territories(campaign.source).roleId
    val nameOfTheAttacker = players.find(_.role.contains(attackersRoleId)).map(_.name).getOrElse("___MISSING_PLAYER___")
    val nameOfTheAttackingTerritory = MapGenerator.myMap(campaign.source)._3
    val linkToCoatOfArmsOfTheAttacker = prefix +  MapGenerator.myMap(campaign.source)._4

    val defendersRoleId = territories(campaign.destination).roleId
    val nameOfTheDefender = players.find(_.role.contains(defendersRoleId)).map(_.name).getOrElse("___MISSING_PLAYER___")
    val nameOfTheDefendingTerritory = MapGenerator.myMap(campaign.destination)._3
    val linkToCoatOfArmsOfTheDefender = prefix +  MapGenerator.myMap(campaign.destination)._4

    div(
      table(border:= "solid 1 px", textAlign:="center",
        thead(
          tr(borderBottom:="solid 1px",
            th("Attacker"),
            th("Defender")
          )
        ),
        tbody(
          tr(borderBottom:="solid 1px",
            td(nameOfTheAttacker),
            td(nameOfTheDefender)
          ),
          tr(borderBottom:="solid 1px",
            td(div(nameOfTheAttackingTerritory)),
            td(div(nameOfTheDefendingTerritory))
          ),
          tr(borderBottom:="solid 1px",
            td(
              img(src:=linkToCoatOfArmsOfTheAttacker)
            ),
            td(
              img(src:=linkToCoatOfArmsOfTheDefender)
            )
          ),
          tr(borderBottom:="dotted 1px",
            td(colSpan:=2, "Total available units:")
          ),
          tr(borderBottom:="solid 1px",
            td(territories(campaign.source).noOfUnits),
            td(territories(campaign.destination).noOfUnits)
          ),
          tr(borderBottom:="dotted 1px",
            td(colSpan:=2, "Total losses so far in this campaign:")
          ),
          tr(borderBottom:="solid 1px",
            td(campaign.noOfTroopsLostByTheAttacker),
            td(campaign.noOfTroopsLostByTheDefender)
          )
        )
      ),

    )
  }

  def battleComponent(battleNumber: Int, battle: ClientBattle): TagOf[Div] = {

    def bb(battle: ClientBattle): TagOf[TableSection] = {
      battle.phase match {
        case DeclareNoOfTroopsPhase => tbody(
          tr(
            td(height:="220px", {
              val noOfAttackers = battle.noOfAttackers
              if (noOfAttackers == 0) {
                "Waiting for the attacker to declare the no. of troops to attack."
              } else {
                noOfAttackers + " attacker" + {if (noOfAttackers > 1) "s" else ""}
              }
            }),
            td(height:="220px", {
              val noOfDefenders = battle.noOfDefenders
              if (battle.noOfAttackers == 0) {
                "---"
              } else if (noOfDefenders == 0) {
                "Waiting for the defender to declare the no. of troops to defend this territory."
              } else {
                "should never happen.."
              }
            })
          ),
          tr(borderTop:="solid 1px", td(colSpan:=2, br())),
          tr(td(colSpan:=2, br()))
        )
        case DiceThrowingPhase | ResultPhase => tbody(
          tr(
            if (battle.attackerDie.isEmpty && !battle.attackerDieRolling) {
              td(height:="220px",
                div(s"Waiting for the attacker to roll ${battle.noOfAttackers}d6.")
              )
            } else {
              td(verticalAlign:="top", height:="220px",
                div(color:="red", fontSize:="-webkit-xxx-large",
                  if (battle.attackerDie.isDefined) {
                    (for (dice <- battle.attackerDie.get) yield {
                      div(integerToDieString(dice))
                    }).toTagMod
                  } else if (battle.attackerDieRolling) {
                    RollingDieComponent.rollingDieComponent(battle.noOfAttackers)
                  } else div()
                )
              )
            },
            if (battle.defenderDie.isEmpty && !battle.defenderDieRolling) {
              td(height:="220px",
                div(s"Waiting for the defender to roll ${battle.noOfDefenders}d6.")
              )
            } else {
              td(verticalAlign:="top", height:="220px",
                div(color:="blue", fontSize:="-webkit-xxx-large",
                  if (battle.defenderDie.isDefined) {
                    (for (dice <- battle.defenderDie.get) yield {
                      div(integerToDieString(dice))
                    }).toTagMod
                  } else if (battle.defenderDieRolling) {
                    RollingDieComponent.rollingDieComponent(battle.noOfDefenders)
                  } else div()
                )
              )
            }
          ),
          Option.when(battle.phase == ResultPhase)(
            tr(borderTop:="solid 1px",
              td(colSpan:=2, "LOSSES:")
            )
          ),
          Option.when(battle.phase == ResultPhase) (
            tr(
              td({
                val x = battle.noOfTroopsLostByTheAttacker
                if (x == 0)
                  "No losses."
                else x.toString
              }),
              td({
                val x = battle.noOfTroopsLostByTheDefender
                if (x == 0)
                  "No losses."
                else x.toString
              }),
            )
          ),
          Option.when(battle.phase != ResultPhase)
          (tr(borderTop:="solid 1px", td(colSpan:=2, br()))),
          Option.when(battle.phase != ResultPhase)
          (tr(td(colSpan:=2, br())))
        )
      }
    }

    div(
      table(border:= "solid 1 px", textAlign:="center", width:="100%",
        colgroup(
          col(VdomAttr("span"):=1, width:="50%"),
          col(VdomAttr("span"):=1, width:="50%")
        ),
        thead(
          tr(borderBottom:="solid 1px",
            th(colSpan:=2, battleNumber + ". battle"),
          )
        ), bb(battle)
      )
    )
  }

  def tabSwitcher(commander: ClientEventHandler[SwitchToTab], selectedTab: Int, campaignIsInProgress: Boolean = false, inventorySize: Int): TagOf[Div] = {
    val mapText = if (selectedTab == 1) "<<< MAP >>>" else "MAP"
    val campaignText = if (selectedTab == 2) "<<< CAMPAIGN >>>" else "CAMPAIGN"
    val inventoryText = if (selectedTab == 3) "<<< INVENTORY (" + inventorySize + ") >>>" else "INVENTORY (" + inventorySize + ")"
    div(display:="flex", flexDirection:="column", width:="220px",
      button(`type` := "button", `class` := "btn btn-primary", mapText, onClick --> Callback {
        commander.addAnEventToTheEventQueue(SwitchToTab(MapTab))
      }),
      button(`type` := "button", `class` := "btn btn-primary", disabled:= !campaignIsInProgress, campaignText, onClick --> Callback {
        commander.addAnEventToTheEventQueue(SwitchToTab(CampaignTab))
      }),
      button(`type` := "button", `class` := "btn btn-primary", inventoryText, onClick --> Callback {
        commander.addAnEventToTheEventQueue(SwitchToTab(InventoryTab))
      }),
    )
  }

  def buttonStack(buttons: List[TagOf[Button]]): TagOf[Div] = {
    div(display:="flex", flexDirection:="column",
      buttons.map(_(marginBottom:="10px")).toTagMod
    )
  }
}
