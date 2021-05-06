package risk.backend

import gbge.backend.{ExecuteEffect, FailureWithMessage, GeneralFailure, OK, UnauthorizedFailure, UniverseResult}
import monocle.{Lens, Optional}
import risk.shared.ClientInnerRisk
import risk.shared.abstract0._
import zio.ZIO

import scala.util.Random

case class InnerRisk private (
                               override val territories: List[TerritoryOccupationInfo],
                               override val players: List[RiskPlayer],
                               override val currentPlayer: Int,
                               override val phase: GamePhase = AddingUnits,
                               override val campaign: Option[Campaign] = None,
                               override val round: Int = 0,
                               override val noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer: Int = 0
                             ) extends AbstractInnerRisk {

  def toClientInnerGame(role: Option[Int] = None): ClientInnerRisk = {
    ClientInnerRisk(territories, players.map(p => {
      if (role.contains(p.role)) {
        p.toOwnersClientRiskPlayer
      } else
        p.toClientRiskPlayer
    }), currentPlayer, phase, campaign.map(_.toClientBattle()), round, noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer)
  }

  override def getPlayerById(playerId: Int): Option[RiskPlayer] = players.find(_.role == playerId)

  override lazy val getCurrentPlayer: RiskPlayer = getPlayerById(currentPlayer).get

  private val nextRoundNumber: Int = {
    val roles = players.filterNot(_.eliminated).map(_.role)
    if (!roles.exists(_ > currentPlayer))
      round +1
    else
      round
  }

  private val nextPlayerId: Int = {
    val roles = players.filterNot(_.eliminated).map(_.role)
    val greaterRoles = roles.filter(_ > currentPlayer)
    greaterRoles.minOption.getOrElse(roles.min)
  }

  private def playerLen(roleId: Int): Optional[List[RiskPlayer], RiskPlayer] = Optional[List[RiskPlayer], RiskPlayer](riskPlayers => {
    riskPlayers.find(_.role == roleId)
  })(riskPlayer => riskPlayers => {
    val index = riskPlayers.zipWithIndex.find(_._1.role == riskPlayer.role).map(_._2)
    if (index.isDefined)
      riskPlayers.updated(index.get, riskPlayer)
    else
      riskPlayers
  })

  private def territoryLen(index: Int): Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo] = Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo](listOfTerritories => {
    listOfTerritories.lift(index)
  })(territory => listOfTerritories => {
    listOfTerritories.updated(index, territory)
  })

  private val sourceTerritoryLen: Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo] = {
    if (campaign.isEmpty)
      Optional.void
    else
      territoryLen(campaign.get.source)
  }

  private val destinationTerritoryLen: Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo] = {
    if (campaign.isEmpty)
      Optional.void
    else
      territoryLen(campaign.get.destination)
  }

  private val currentPlayerLen: Lens[List[RiskPlayer], RiskPlayer] = Lens[List[RiskPlayer], RiskPlayer](riskPlayers => {
    riskPlayers.find(_.role == currentPlayer).get
  })(rp => riskPlayers => {
    val index = riskPlayers.zipWithIndex.find(_._1.role == currentPlayer).get._2
    riskPlayers.updated(index, rp)
  })

  val territoryJustGotConquered: Boolean = {
    if (campaign.isDefined && campaign.get.battle.phase == ResultPhase) {
      val defendingTerritory = territories(campaign.get.destination)
      defendingTerritory.noOfUnits == 0
    } else false
  }

  def reduce(riskAction: RiskAction, invokerRole: Option[Int], isAdmin: Boolean = false): (InnerRisk, UniverseResult) = {

    val isTheInvokerTheCurrentPlayer: Boolean = invokerRole.contains(this.currentPlayer)

    def checkStuffForTerritory(targetTerritory: Int): Option[FailureWithMessage] = {
      val territoryExists: Boolean = targetTerritory >= 0 && targetTerritory < territories.size
      if (!isTheInvokerTheCurrentPlayer) {
        Some(UnauthorizedFailure("Authentication error."))
      }
      else if (!territoryExists) {
        Some(GeneralFailure("Target territory does not exists."))
      } else {
        val doesTerritoryBelongToInvoker: Boolean = invokerRole.contains(territories(targetTerritory).roleId)
        val isItTheTurnOfTheInvoker: Boolean = invokerRole.contains(currentPlayer)
        if (!isItTheTurnOfTheInvoker) {
          Some(GeneralFailure("It is not the turn of the invoker."))
        }
        else if (!doesTerritoryBelongToInvoker) {
          Some(GeneralFailure("Territory does not belong to invoker."))
        } else {
          None
        }
      }
    }

    riskAction match {
      case toggle :Toggle => {
        val roleId = toggle.roleId
        val riskPlayer: Option[RiskPlayer] = players.find(_.role == roleId)
        if (riskPlayer.isEmpty) {
          (this, GeneralFailure("No such player with that id."))
        } else if (invokerRole.isEmpty) {
          (this, UnauthorizedFailure("Authentication required."))
        } else if (!invokerRole.contains(roleId)) {
          (this, UnauthorizedFailure("Only the player can modify this setting."))
        } else {
          val player = riskPlayer.get
          val modifiedPlayer = toggle match {
            case ToggleRollAttackerDieAutomatically(_) =>
              player.copy(rollAttackerDieAutomatically = !player.rollAttackerDieAutomatically)
            case ToggleRollDefenderDieAutomatically(_) =>
              player.copy(rollDefenderDieAutomatically = !player.rollDefenderDieAutomatically)
            case ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent(_) =>
              player.copy(defendWithOneAutomaticallyIfOnlyOneUnitIsPresent = !player.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent)
          }
          (this.copy(players = playerLen(roleId).modify(_ => modifiedPlayer)(players)), OK)
        }
      }
      case AddUnitToTerritory(targetTerritory) => {
        val error = checkStuffForTerritory(targetTerritory)
        if (error.isDefined) {
          (this, error.get)
        } else {
          phase match {
            case AddingUnits => {
              val player = getPlayerById(invokerRole.get)
              assert(player.isDefined)
              assert(player.get.troops > 0)
              val updatedPlayer = player.get.copy(troops = player.get.troops - 1)
              val temp: TerritoryOccupationInfo = territories(targetTerritory)
              val temp2 = temp.copy(noOfUnits = temp.noOfUnits+1)
              val newPlayers = players.map(player => if (player.role == invokerRole.get) updatedPlayer else player)
              val nextPhase = if (newPlayers.forall(_.troops ==0)) DeployingUnits else AddingUnits
              val tempx: InnerRisk = this.copy(
                territories = territories.updated(targetTerritory, temp2),
                players = newPlayers,
                currentPlayer = nextPlayerId,
                phase = nextPhase
              )
              if (nextPhase == AddingUnits) {
                (tempx, OK)
              } else {
                val newPlayer = tempx.getCurrentPlayer.copy(troops = tempx.calculateNoOfNewTroops)
                val newPlayers = tempx.players.map({
                  case RiskPlayer(newPlayer.role, _, _, _, _, _, _, _) => newPlayer
                  case x => x
                })
                (tempx.copy(players = newPlayers, round = 1), OK)
              }
            }
            case _ => (this, GeneralFailure("This action can't be used in the current phase."))
          }
        }
      }
      case DeployUnitToTerritory(targetTerritory) => {
        val error = checkStuffForTerritory(targetTerritory)
        if (error.isDefined) {
          (this, error.get)
        } else {
          phase match {
            case DeployingUnits => {
              val player = currentPlayerLen.get(players)
              if (player.troops <=0) {
                (this, GeneralFailure("Cannot deploy more troops, you have ran out of reserves."))
              } else {
                val updatedPlayer = player.copy(troops = player.troops - 1)
                val temp: TerritoryOccupationInfo = territories(targetTerritory)
                val temp2 = temp.copy(noOfProposedUnits = temp.noOfProposedUnits + 1)
                val newPlayers = players.map(player => if (player.role == invokerRole.get) updatedPlayer else player)
                (this.copy(
                  territories = territories.updated(targetTerritory, temp2),
                  players = newPlayers), OK)
              }
            }
            case _ => (this, GeneralFailure("This action can't be used in the current phase."))
          }
        }
      }
      case RetractUnitFromTerritory(targetTerritory) => {
        val error = checkStuffForTerritory(targetTerritory)
        if (error.isDefined) {
          (this, error.get)
        } else {
          phase match {
            case DeployingUnits => {
              val player = currentPlayerLen.get(players)
              val t3: TerritoryOccupationInfo = territories(targetTerritory)
              if (t3.noOfProposedUnits < 1) {
                (this, GeneralFailure("There are no troops to retract."))
              } else {
                val updatedPlayer = player.copy(troops = player.troops + 1)
                val temp2 = t3.copy(noOfProposedUnits = t3.noOfProposedUnits - 1)
                val newPlayers = players.map(player => if (player.role == invokerRole.get) updatedPlayer else player)
                (this.copy(
                  territories = territories.updated(targetTerritory, temp2),
                  players = newPlayers), OK)
              }
            }
            case _ => (this, GeneralFailure("This action can't be used in the current phase."))
          }
        }
      }
      case CommitDeployment => {
        if (!invokerRole.contains(this.currentPlayer)) {
          (this, GeneralFailure("This is not your turn."))
        } else {
          phase match {
            case DeployingUnits => {
              val player = currentPlayerLen.get(players)
              if (player.troops != 0) {
                (this, GeneralFailure("You have to deploy all your troops before commit."))
              } else {
                (copy(
                  phase = BattleAndManeuver,
                  territories = territories.map(t=> t.copy(noOfUnits = t.noOfUnits + t.noOfProposedUnits, noOfProposedUnits = 0))
                ), OK)
              }
            }
            case _ => (this, GeneralFailure("This action can't be used in the current phase."))
          }
        }
      }
      case LaunchCampaign(source, destination) => {
        if (phase != BattleAndManeuver) {
          (this, GeneralFailure("Wrong phase."))
        } else if (!invokerRole.contains(this.currentPlayer)) {
          (this, GeneralFailure("This is not your turn."))
        } else {
          val role = invokerRole.get
          if (this.campaign.isDefined && campaign.exists(_.campaignStatus == RUNNING) || campaign.exists(_.campaignStatus == DEFENDER_LOST)) {
            (this, GeneralFailure("The current campaign needs to be resolved first."))
          } else if (this.territories(source).roleId != role) {
            (this, GeneralFailure("You may only attack from a territory that you control."))
          } else if (this.territories(destination).roleId == role) {
            (this, GeneralFailure("You cannot attack a territory which is already under your control."))
          } else if (!areTheyNeighbours(source, destination)) {
            (this, GeneralFailure("Not neighbours."))
          } else {
            (copy(
              campaign = Some(Campaign(source, destination, Battle(), 1, 0, 0)),
              territories = destinationTerritoryLen.modify(_.normalize())(territories)
            ), OK)
          }
        }
      }
      case bca: CampaignAction => {
        if (phase != BattleAndManeuver) {
          (this, GeneralFailure("Wrong phase."))
        } else if (campaign.isEmpty) {
          (this, GeneralFailure("There is no  current campaign..."))
        } else {
          val campaignBefore = this.campaign.get
          val (campaignAfter, uResult) = campaignBefore.reduce(bca)
          bca match {
            case Attack(noOfAttackers) => {
              if (!invokerRole.contains(attackersRole)) {
                (this, GeneralFailure("The 'Attack' action can only be initiated by the attacker."))
              } else {
                val destinationTerritory = destinationTerritoryLen.getOption(territories).get
                val destinationPlayer = players.find(_.role == destinationTerritory.roleId).get
                val result =
                  if (destinationPlayer.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent && destinationTerritory.noOfUnits ==1)
                    ExecuteEffect(_ => ZIO.succeed(List(Defend(1))))
                  else
                    OK
                (this.copy(
                  campaign = Some(campaignAfter),
                  territories = sourceTerritoryLen.modify(_.copy(noOfUnitsInBattle = noOfAttackers))(territories)
                ), result)
              }
            }
            case Defend(noOfDefenders) => {
              val autoDefend: Boolean = {
                val targetTerritory = this.destinationTerritoryLen.getOption(this.territories).get
                val defendingPlayer = players.find(_.role == targetTerritory.roleId).get
                defendingPlayer.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent && targetTerritory.noOfUnits == 1
              }
              if (!invokerRole.contains(defendersRole) && !autoDefend) {
                (this, UnauthorizedFailure("The 'Defend' action can either be initiated by the defender or can be triggered by autodefend"))
              } else {
                val sourceTerritory = sourceTerritoryLen.getOption(territories).get
                val sourcePlayer = players.find(_.role == sourceTerritory.roleId).get

                val destinationTerritory = destinationTerritoryLen.getOption(territories).get
                val destinationPlayer = players.find(_.role == destinationTerritory.roleId).get

                val extraAction1: Option[RiskAction] =
                  if (sourcePlayer.rollAttackerDieAutomatically) Some(RollDiceForAttacker) else None

                val extraAction2: Option[RiskAction] =
                  if (destinationPlayer.rollDefenderDieAutomatically) Some(RollDiceForDefender) else None

                val extraActions = extraAction1.toList ::: extraAction2.toList

                val result = if (extraActions.isEmpty) OK else ExecuteEffect(_ => ZIO.succeed(extraActions))

                (this.copy(
                  campaign = Some(campaignAfter),
                  territories = destinationTerritoryLen.modify(_.copy(noOfUnitsInBattle = noOfDefenders))(territories)
                ), result)
              }
            }
            case RollDiceForAttacker | RollDiceForDefender => {
              (this.copy(campaign = Some(campaignAfter)), uResult)
            }
            case DiceResultForAttacker(_) | DiceResultForDefender(_) => {
              if (campaignAfter.battle.phase == ResultPhase) {
                val tempTerritories: List[TerritoryOccupationInfo] = destinationTerritoryLen.modify(_.resolveBattle(
                  campaignAfter.battle.noOfTroopsLostByTheDefender,
                  campaignAfter.battle.remainingDefendingTroops))(territories)
                val updatedTerritories: List[TerritoryOccupationInfo] = sourceTerritoryLen.modify(_.resolveBattle(
                  campaignAfter.battle.noOfTroopsLostByTheAttacker,
                  campaignAfter.battle.remainingAttackingTroops,
                ))(tempTerritories)
                val newStatus =
                  if (destinationTerritoryLen.getOption(updatedTerritories).exists(_.noOfUnits == 0))
                    DEFENDER_LOST
                  else if (sourceTerritoryLen.getOption(updatedTerritories).exists(_.noOfUnits == 1))
                    ATTACKER_LOST
                  else
                    RUNNING

                val counter = {
                  if (newStatus == DEFENDER_LOST)
                    noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer + 1
                  else
                    noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer
                }
                val reward: Option[String] =
                  if (noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer == 0 && counter == 1)
                    Some("Trophy for conquering " + ts(campaignAfter.destination).name + " from " + ts(campaignAfter.source).name + " in round #" + round + ".")
                  else
                    None

                (this.copy(
                  campaign = Some(campaignAfter.copy(campaignStatus = newStatus)),
                  territories = updatedTerritories,
                  noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer = counter,
                  players = currentPlayerLen.modify(cp => cp.copy(inventory = cp.inventory ::: reward.toList))(players)
                ), OK)
              } else {
                (this.copy(campaign = Some(campaignAfter)), OK)
              }
            }
          }
        }
      }
      case MoveInToConqueredTerritory(noOfTroops) => {
        if (!territoryJustGotConquered) {
          (this, GeneralFailure("Wrong phase"))
        } else {
          val battle = this.campaign.get
          val min = battle.battle.noOfAttackers
          val max = territories(battle.source).noOfUnits - 1
          if (noOfTroops > max)
            (this, GeneralFailure("You cannot move in that many troops."))
          else if (noOfTroops < min)
            (this, GeneralFailure(s"You have to move in at least $min troops."))
          else {
            val tempTerritories: List[TerritoryOccupationInfo] = sourceTerritoryLen.modify(x =>
              x.copy(noOfUnits = x.noOfUnits - noOfTroops, noOfUnitsInBattle = 0))(territories)
            val updatedTerritories: List[TerritoryOccupationInfo] = destinationTerritoryLen.modify(_.copy(
              noOfUnits = noOfTroops, noOfUnitsInBattle = 0, roleId = attackersRole))(tempTerritories)

            val np = this.players.map(player => {
              if (!player.eliminated && !updatedTerritories.exists(_.roleId == player.role)) {
                player.copy(eliminated = true)
              } else {
                player
              }
            })
            val newPhase = if (np.filterNot(_.eliminated).size == 1) GameOver else this.phase
            (this.copy(players = np, territories = updatedTerritories, campaign = None, phase = newPhase), OK)
          }
        }
      }
      case AbandonCurrentCampaign => {
        if (campaign.isEmpty || campaign.get.campaignStatus != RUNNING || campaign.get.battle.phase != ResultPhase)
          (this, GeneralFailure("___"))
        else {
          val updatedCampaign = this.campaign.get.copy(campaignStatus = ABANDONED)
          val t1 = territoryLen(updatedCampaign.source).modify(_.normalize())(territories)
          val t2 = territoryLen(updatedCampaign.destination).modify(_.normalize())(t1)
          (this.copy(campaign = Some(updatedCampaign), territories = t2), OK)
        }
      }
      case ManeuverTroops(from, to, noOfTroops) => {
        if (!isTheInvokerTheCurrentPlayer) {
          (this, UnauthorizedFailure("Authentication error."))
        } else if (phase != BattleAndManeuver) {
          (this, GeneralFailure("Wrong phase"))
        } else if (campaign.map(_.campaignStatus).contains(RUNNING)) {
          (this, GeneralFailure("The current campaign needs to be resolved first."))
        } else if (campaign.map(_.campaignStatus).contains(DEFENDER_LOST)) {
          (this, GeneralFailure("First you need to move in at least 1 troop to the conquered territory."))
        } else if (!invokerRole.contains(territories(from).roleId)) {
          (this, GeneralFailure("Source is not under your command."))
        } else if (!invokerRole.contains(territories(to).roleId)) {
          (this, GeneralFailure("Destination is not under your command."))
        } else if (noOfTroops >= territories(from).noOfUnits) {
          (this, GeneralFailure("Too many troops"))
        } else if (!areTheyConnected(from, to)) {
          (this, GeneralFailure("Territories are not connected."))
        } else {
          val t0 = if (campaign.isDefined) {
            val t11 = territoryLen(campaign.get.source).modify(_.normalize())(territories)
            territoryLen(campaign.get.destination).modify(_.normalize())(t11)
          } else territories
          val t1 = territoryLen(from).modify(t => t.copy(noOfUnits = t.noOfUnits - noOfTroops).normalize())(t0)
          val t2 = territoryLen(to).modify(t => t.copy(noOfUnits = t.noOfUnits + noOfTroops).normalize())(t1)
          val temp = this.copy(territories = t2, currentPlayer = nextPlayerId, phase = DeployingUnits, campaign = None, round = nextRoundNumber, noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer = 0)
          (temp.copy(players = temp.currentPlayerLen.modify(_.copy(troops = temp.calculateNoOfNewTroops))(temp.players)), OK)
        }
      }
      case Skip => {
        if (!invokerRole.contains(this.currentPlayer)) {
          (this, GeneralFailure("This is not your turn."))
        } else if (phase != BattleAndManeuver) {
          (this, GeneralFailure("Cannot skip in this phase."))
        } else if (campaign.exists(_.campaignStatus == RUNNING) && campaign.get.battle.phase != ResultPhase) {
          (this, GeneralFailure("Cannot skip round, because your current campaign needs to be resolved first."))
        } else {
          val temp = copy(phase = DeployingUnits, currentPlayer = nextPlayerId, campaign = None, noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer = 0,
            territories = destinationTerritoryLen.modify(_.normalize())(sourceTerritoryLen.modify(_.normalize())(territories)))
          val temp2 = temp.copy(players = temp.currentPlayerLen.modify(_.copy(troops = temp.calculateNoOfNewTroops))(temp.players), round = nextRoundNumber)
          (temp2, OK)
        }
      }
      case _ => {
        (this, GeneralFailure("Unsupported action"))
      }
    }
  }
}

object InnerRisk {
  def generate(colorMap: ColorMap, seed: Long): InnerRisk = {
    val riskPlayers: List[RiskPlayer] = colorMap.colorMap.map(entry => {
      RiskPlayer(entry._2, entry._1, 10)
    }).toList.sortBy(_.role)
    val initialTerritoryDivision = (for (i <- 1 to 20) yield {
      val modulo = colorMap.size - ((i-1) % colorMap.size) - 1
      TerritoryOccupationInfo(riskPlayers(modulo).role, 1)
    }).toList
    InnerRisk(
      territories = new Random(seed).shuffle(initialTerritoryDivision),
      riskPlayers,
      riskPlayers.map(_.role).min
    )
  }
}
