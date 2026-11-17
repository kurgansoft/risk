package risk.backend

import gbge.backend.{
  Failure,
  FailureWithMessage,
  GeneralFailure,
  UnauthorizedFailure
}
import risk.shared.ClientInnerRisk
import risk.shared.abstract0.*
import zio.blocks.schema.*
import risk.shared.abstract0.GamePhase.*

import scala.language.implicitConversions
import scala.util.Random

case class InnerRisk private[backend] (
                                        override val territories: List[TerritoryOccupationInfo],
                                        override val players: Map[RiskRole, RiskPlayerInfo],
                                        override val roleOfTheCurrentPlayer: RiskRole,
                                        override val phase: GamePhase = AddingUnits,
                                        override val campaign: Option[Campaign] = None,
                                        override val round: Int = 0,
                                        override val noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer: Int = 0
) extends AbstractInnerRisk(
      territories,
      players,
      roleOfTheCurrentPlayer,
      phase,
      campaign,
      round,
      noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer
    ) {

  override lazy val getCurrentPlayer: RiskPlayerInfo =
    players(roleOfTheCurrentPlayer)

  implicit def convert(innerRisk: InnerRisk): Either[Failure, InnerRisk] =
    Right(innerRisk)

  def toClientInnerGame(
      roleOfTheInvoker: Option[RiskRole] = None
  ): ClientInnerRisk = {
    ClientInnerRisk(
      territories,
      players.map({ case (role, playerInfo) =>
        if (roleOfTheInvoker.contains(role)) {
          role -> playerInfo.toOwnersClientRiskPlayer
        } else
          role -> playerInfo.toClientRiskPlayer
      }),
      roleOfTheCurrentPlayer,
      phase,
      campaign.map(_.toClientBattle()),
      round,
      noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer
    )
  }

  private val nextRoundNumber: Int = {
    val roles = activeRoles
    if (roles.lastOption.contains(roleOfTheCurrentPlayer))
      round + 1
    else
      round
  }

  private def activeRoles: List[RiskRole] =
    players.iterator
      .collect { case (role, player) if !player.eliminated => role }
      .toList
      .sortBy(_.roleId)

  private lazy val nextPlayerId: RiskRole = {
    val roles = activeRoles
    val index = roles.indexOf(roleOfTheCurrentPlayer)
    roles((index + 1) % roles.size)
  }

  private lazy val sourceTerritoryLen
      : Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo] = {
    val territoryIndex = campaign.map(_.source).getOrElse(999999)
    InnerRisk.territoryLen(territoryIndex)
  }

  private lazy val destinationTerritoryLen
      : Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo] = {
    val territoryIndex = campaign.map(_.destination).getOrElse(999999)
    InnerRisk.territoryLen(territoryIndex)
  }

  private val territoryJustGotConquered: Boolean = {
    if (campaign.isDefined && campaign.get.battle.phase == ResultPhase) {
      val defendingTerritory = territories(campaign.get.destination)
      defendingTerritory.noOfUnits == 0
    } else false
  }

  def reduce(
      riskAction: RiskAction,
      invokerRole: RiskRole
  ): Either[Failure, InnerRisk] = {

    val isTheInvokerTheCurrentPlayer: Boolean =
      invokerRole == roleOfTheCurrentPlayer

    def checkStuffForTerritory(
        targetTerritory: Int
    ): Option[FailureWithMessage] = {
      val territoryExists: Boolean =
        targetTerritory >= 0 && targetTerritory < territories.size
      if (!players.contains(invokerRole)) {
        Some(UnauthorizedFailure("Authentication error."))
      } else if (!territoryExists) {
        Some(GeneralFailure("Target territory does not exists."))
      } else {
        val doesTerritoryBelongToInvoker: Boolean =
          invokerRole == territories(targetTerritory).role
        val isItTheTurnOfTheInvoker: Boolean =
          invokerRole == roleOfTheCurrentPlayer
        if (!isItTheTurnOfTheInvoker) {
          Some(GeneralFailure("It is not the turn of the invoker."))
        } else if (!doesTerritoryBelongToInvoker) {
          Some(GeneralFailure("Territory does not belong to invoker."))
        } else {
          None
        }
      }
    }

    riskAction match {
      case toggle: Toggle =>
        if (toggle.role != invokerRole || !players.contains(invokerRole)) {
          Left(UnauthorizedFailure("Only the player can modify this setting."))
        } else {
          val player = players(invokerRole)
          val modifiedPlayer = toggle match {
            case ToggleRollAttackerDieAutomatically(_) =>
              player.copy(rollAttackerDieAutomatically =
                !player.rollAttackerDieAutomatically
              )
            case ToggleRollDefenderDieAutomatically(_) =>
              player.copy(rollDefenderDieAutomatically =
                !player.rollDefenderDieAutomatically
              )
            case ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent(_) =>
              player.copy(defendWithOneAutomaticallyIfOnlyOneUnitIsPresent =
                !player.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent
              )
          }
          this.copy(players = players.updated(invokerRole, modifiedPlayer))
        }
      case AddUnitToTerritory(targetTerritory) =>
        val error = checkStuffForTerritory(targetTerritory)
        if (error.isDefined) {
          Left(error.get)
        } else {
          phase match {
            case AddingUnits =>
              val player = players(invokerRole)
              if (player.troops <= 0) {
                Left(GeneralFailure("You have no troops left to place."))
              } else {
                val updatedPlayer = player.copy(troops = player.troops - 1)
                val temp = territories(targetTerritory)
                val temp2 = temp.copy(noOfUnits = temp.noOfUnits + 1)
                val newPlayers = players.updated(invokerRole, updatedPlayer)
                val nextPhase =
                  if (newPlayers.values.forall(_.troops == 0)) DeployingUnits
                  else AddingUnits
                val tempx = this.copy(
                  territories = territories.updated(targetTerritory, temp2),
                  players = newPlayers,
                  roleOfTheCurrentPlayer = nextPlayerId,
                  phase = nextPhase
                )
                if (nextPhase == AddingUnits) {
                  tempx
                } else {
                  val newPlayer = tempx.getCurrentPlayer.copy(troops =
                    tempx.calculateNoOfNewTroops
                  )
                  val newPlayers = tempx.players.updated(
                    tempx.roleOfTheCurrentPlayer,
                    newPlayer
                  )
                  tempx.copy(players = newPlayers, round = 1)
                }
              }
            case _ =>
              Left(
                GeneralFailure(
                  "This action can't be used in the current phase."
                )
              )
          }
        }
      case DeployUnitToTerritory(targetTerritory) =>
        val error = checkStuffForTerritory(targetTerritory)
        if (error.isDefined) {
          Left(error.get)
        } else {
          phase match {
            case DeployingUnits =>
              val player = getCurrentPlayer
              if (player.troops <= 0) {
                Left(
                  GeneralFailure(
                    "Cannot deploy more troops, you have ran out of reserves."
                  )
                )
              } else {
                val updatedPlayer = player.copy(troops = player.troops - 1)
                val temp: TerritoryOccupationInfo = territories(targetTerritory)
                val temp2 =
                  temp.copy(noOfProposedUnits = temp.noOfProposedUnits + 1)
                val newPlayers = players.updated(invokerRole, updatedPlayer)
                this.copy(
                  territories = territories.updated(targetTerritory, temp2),
                  players = newPlayers
                )
              }
            case _ =>
              Left(
                GeneralFailure(
                  "This action can't be used in the current phase."
                )
              )
          }
        }
      case RetractUnitFromTerritory(targetTerritory) =>
        val error = checkStuffForTerritory(targetTerritory)
        if (error.isDefined) {
          Left(error.get)
        } else {
          phase match {
            case DeployingUnits =>
              val player = getCurrentPlayer
              val t3: TerritoryOccupationInfo = territories(targetTerritory)
              if (t3.noOfProposedUnits < 1) {
                Left(GeneralFailure("There are no troops to retract."))
              } else {
                val updatedPlayer = player.copy(troops = player.troops + 1)
                val temp2 =
                  t3.copy(noOfProposedUnits = t3.noOfProposedUnits - 1)
                val newPlayers = players.updated(invokerRole, updatedPlayer)
                this.copy(
                  territories = territories.updated(targetTerritory, temp2),
                  players = newPlayers
                )
              }
            case _ =>
              Left(
                GeneralFailure(
                  "This action can't be used in the current phase."
                )
              )
          }
        }
      case CommitDeployment =>
        if (invokerRole != roleOfTheCurrentPlayer) {
          Left(GeneralFailure("This is not your turn."))
        } else {
          phase match {
            case DeployingUnits =>
              val player = getCurrentPlayer
              if (player.troops != 0) {
                Left(
                  GeneralFailure(
                    "You have to deploy all your troops before commit."
                  )
                )
              } else {
                copy(
                  phase = BattleAndManeuver,
                  territories = territories.map(t =>
                    t.copy(
                      noOfUnits = t.noOfUnits + t.noOfProposedUnits,
                      noOfProposedUnits = 0
                    )
                  )
                )
              }
            case _ =>
              Left(
                GeneralFailure(
                  "This action can't be used in the current phase."
                )
              )
          }
        }
      case LaunchCampaign(source, destination) =>
        if (phase != BattleAndManeuver) {
          Left(GeneralFailure("Wrong phase."))
        } else if (invokerRole != roleOfTheCurrentPlayer) {
          Left(GeneralFailure("This is not your turn."))
        } else if (
          source < 0 || source >= territories.size || destination < 0 ||
          destination >= territories.size
        ) {
          Left(GeneralFailure("Invalid territory."))
        } else {
          val role = invokerRole
          if (
            this.campaign.isDefined && campaign.exists(
              _.campaignStatus == RUNNING
            ) || campaign.exists(_.campaignStatus == DEFENDER_LOST)
          ) {
            Left(
              GeneralFailure("The current campaign needs to be resolved first.")
            )
          } else if (this.territories(source).role != role) {
            Left(
              GeneralFailure(
                "You may only attack from a territory that you control."
              )
            )
          } else if (this.territories(destination).role == role) {
            Left(
              GeneralFailure(
                "You cannot attack a territory which is already under your control."
              )
            )
          } else if (!areTheyNeighbours(source, destination)) {
            Left(GeneralFailure("Not neighbours."))
          } else {
            copy(
              campaign = Some(Campaign(source, destination, Battle(), 1, 0, 0)),
              territories =
                destinationTerritoryLen.modify(territories, _.normalize())
            )
          }
        }
      case bca: CampaignAction =>
        if (phase != BattleAndManeuver) {
          Left(GeneralFailure("Wrong phase."))
        } else if (campaign.isEmpty) {
          Left(GeneralFailure("There is no  current campaign..."))
        } else {
          val campaignBefore = this.campaign.get
          campaignBefore.reduce(bca) match {
            case Left(failure)        => Left(failure)
            case Right(campaignAfter) =>
              bca match {
                case Attack(noOfAttackers) =>
                  if (invokerRole != attackersRole) {
                    Left(
                      GeneralFailure(
                        "The 'Attack' action can only be initiated by the attacker."
                      )
                    )
                  } else {
                    this.copy(
                      campaign = Some(campaignAfter),
                      territories = sourceTerritoryLen.modify(
                        territories,
                        _.copy(noOfUnitsInBattle = noOfAttackers)
                      )
                    )
                  }
                case Defend(noOfDefenders) =>
                  val autoDefend: Boolean = {
                    val targetTerritory = this.destinationTerritoryLen
                      .getOption(this.territories)
                      .get
                    val defendingPlayer =
                      players(targetTerritory.role)
                    defendingPlayer.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent && targetTerritory.noOfUnits == 1
                  }
                  if (invokerRole != defendersRole && !autoDefend) {
                    Left(
                      UnauthorizedFailure(
                        "The 'Defend' action can either be initiated by the defender or can be triggered by autodefend"
                      )
                    )
                  } else {
                    this.copy(
                      campaign = Some(campaignAfter),
                      territories = destinationTerritoryLen.modify(
                        territories,
                        _.copy(noOfUnitsInBattle = noOfDefenders)
                      )
                    )
                  }
                case RollDiceForAttacker | RollDiceForDefender =>
                  this.copy(campaign = Some(campaignAfter))
                case DiceResultForAttacker(_) | DiceResultForDefender(_) =>
                  if (campaignAfter.battle.phase == ResultPhase) {
                    val tempTerritories: List[TerritoryOccupationInfo] =
                      destinationTerritoryLen.modify(
                        territories,
                        _.resolveBattle(
                          campaignAfter.battle.noOfTroopsLostByTheDefender,
                          campaignAfter.battle.remainingDefendingTroops
                        )
                      )
                    val updatedTerritories: List[TerritoryOccupationInfo] =
                      sourceTerritoryLen.modify(
                        tempTerritories,
                        _.resolveBattle(
                          campaignAfter.battle.noOfTroopsLostByTheAttacker,
                          campaignAfter.battle.remainingAttackingTroops
                        )
                      )
                    val newStatus =
                      if (
                        destinationTerritoryLen
                          .getOption(updatedTerritories)
                          .exists(_.noOfUnits == 0)
                      )
                        DEFENDER_LOST
                      else if (
                        sourceTerritoryLen
                          .getOption(updatedTerritories)
                          .exists(_.noOfUnits == 1)
                      )
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
                      if (
                        noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer == 0 && counter == 1
                      )
                        Some(
                          "Trophy for conquering " + ts(
                            campaignAfter.destination
                          ).name + " from " + ts(
                            campaignAfter.source
                          ).name + " in round #" + round + "."
                        )
                      else
                        None

                    this.copy(
                      campaign =
                        Some(campaignAfter.copy(campaignStatus = newStatus)),
                      territories = updatedTerritories,
                      noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer =
                        counter,
                      players = players.updated(
                        roleOfTheCurrentPlayer,
                        getCurrentPlayer.copy(
                          inventory =
                            getCurrentPlayer.inventory ::: reward.toList
                        )
                      )
                    )
                  } else {
                    this.copy(campaign = Some(campaignAfter))
                  }
              }
          }
        }
      case MoveInToConqueredTerritory(noOfTroops) =>
        if (!territoryJustGotConquered) {
          Left(GeneralFailure("Wrong phase"))
        } else {
          val battle = this.campaign.get
          val min = battle.battle.noOfAttackers
          val max = territories(battle.source).noOfUnits - 1
          if (noOfTroops > max)
            Left(GeneralFailure("You cannot move in that many troops."))
          else if (noOfTroops < min)
            Left(GeneralFailure(s"You have to move in at least $min troops."))
          else {
            val tempTerritories: List[TerritoryOccupationInfo] =
              sourceTerritoryLen.modify(
                territories,
                x =>
                  x.copy(
                    noOfUnits = x.noOfUnits - noOfTroops,
                    noOfUnitsInBattle = 0
                  )
              )
            val updatedTerritories: List[TerritoryOccupationInfo] =
              destinationTerritoryLen.modify(
                tempTerritories,
                _.copy(
                  noOfUnits = noOfTroops,
                  noOfUnitsInBattle = 0,
                  role = attackersRole
                )
              )

            val np = this.players.map { case (role, player) =>
              if (
                !player.eliminated && !updatedTerritories
                  .exists(_.role == role)
              ) {
                role -> player.copy(eliminated = true)
              } else {
                role -> player
              }
            }
            val newPhase =
              if (np.values.count(player => !player.eliminated) == 1) GameOver
              else this.phase
            this.copy(
              players = np,
              territories = updatedTerritories,
              campaign = None,
              phase = newPhase
            )
          }
        }
      case AbandonCurrentCampaign =>
        if (
          campaign.isEmpty || campaign.get.campaignStatus != RUNNING || campaign.get.battle.phase != ResultPhase
        )
          Left(GeneralFailure("___"))
        else {
          val updatedCampaign =
            this.campaign.get.copy(campaignStatus = ABANDONED)
          val t1 = InnerRisk
            .territoryLen(updatedCampaign.source)
            .modify(territories, _.normalize())
          val t2 = InnerRisk
            .territoryLen(updatedCampaign.destination)
            .modify(t1, _.normalize())
          this.copy(campaign = Some(updatedCampaign), territories = t2)
        }
      case ManeuverTroops(from, to, noOfTroops) =>
        if (!isTheInvokerTheCurrentPlayer) {
          Left(UnauthorizedFailure("Authentication error."))
        } else if (phase != BattleAndManeuver) {
          Left(GeneralFailure("Wrong phase"))
        } else if (campaign.map(_.campaignStatus).contains(RUNNING)) {
          Left(
            GeneralFailure("The current campaign needs to be resolved first.")
          )
        } else if (campaign.map(_.campaignStatus).contains(DEFENDER_LOST)) {
          Left(
            GeneralFailure(
              "First you need to move in at least 1 troop to the conquered territory."
            )
          )
        } else if (
          from < 0 || from >= territories.size || to < 0 ||
          to >= territories.size
        ) {
          Left(GeneralFailure("Invalid territory."))
        } else if (invokerRole != territories(from).role) {
          Left(GeneralFailure("Source is not under your command."))
        } else if (invokerRole != territories(to).role) {
          Left(GeneralFailure("Destination is not under your command."))
        } else if (noOfTroops >= territories(from).noOfUnits) {
          Left(GeneralFailure("Too many troops"))
        } else if (!areTheyConnected(from, to)) {
          Left(GeneralFailure("Territories are not connected."))
        } else {
          val t0 = if (campaign.isDefined) {
            val t11 = InnerRisk
              .territoryLen(campaign.get.source)
              .modify(territories, _.normalize())
            InnerRisk
              .territoryLen(campaign.get.destination)
              .modify(t11, _.normalize())
          } else territories
          val t1 = InnerRisk
            .territoryLen(from)
            .modify(
              t0,
              t => t.copy(noOfUnits = t.noOfUnits - noOfTroops).normalize()
            )
          val t2 = InnerRisk
            .territoryLen(to)
            .modify(
              t1,
              t => t.copy(noOfUnits = t.noOfUnits + noOfTroops).normalize()
            )
          val temp = this.copy(
            territories = t2,
            roleOfTheCurrentPlayer = nextPlayerId,
            phase = DeployingUnits,
            campaign = None,
            round = nextRoundNumber,
            noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer = 0
          )
          temp.copy(players =
            temp.players.updated(
              temp.roleOfTheCurrentPlayer,
              temp.getCurrentPlayer.copy(troops = temp.calculateNoOfNewTroops)
            )
          )
        }
      case Skip =>
        if (invokerRole != roleOfTheCurrentPlayer) {
          Left(GeneralFailure("This is not your turn."))
        } else if (phase != BattleAndManeuver) {
          Left(GeneralFailure("Cannot skip in this phase."))
        } else if (
          campaign.exists(
            _.campaignStatus == RUNNING
          ) && campaign.get.battle.phase != ResultPhase
        ) {
          Left(
            GeneralFailure(
              "Cannot skip round, because your current campaign needs to be resolved first."
            )
          )
        } else {
          val temp = copy(
            phase = DeployingUnits,
            roleOfTheCurrentPlayer = nextPlayerId,
            campaign = None,
            noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer = 0,
            territories = destinationTerritoryLen.modify(
              sourceTerritoryLen.modify(territories, _.normalize()),
              _.normalize()
            )
          )
          val temp2 = temp.copy(
            players = temp.players.updated(
              temp.roleOfTheCurrentPlayer,
              temp.getCurrentPlayer.copy(troops = temp.calculateNoOfNewTroops)
            ),
            round = nextRoundNumber
          )
          temp2
        }
      case _ =>
        Left(GeneralFailure("Unsupported action"))
    }
  }
}

object InnerRisk {
  def generate(roles: Set[RiskRole], seed: Long): InnerRisk = {
    val riskPlayers: Map[RiskRole, RiskPlayerInfo] = roles
      .map(role => role -> RiskPlayerInfo(10))
      .toMap
    val orderedRoles: List[RiskRole] = roles.toList.sortBy(_.roleId)
    val initialTerritoryDivision = (for (i <- 1 to 20) yield {
      val modulo = roles.size - ((i - 1) % roles.size) - 1
      TerritoryOccupationInfo(orderedRoles(modulo), 1)
    }).toList
    InnerRisk(
      territories = new Random(seed).shuffle(initialTerritoryDivision),
      riskPlayers,
      roles.minBy(_.roleId)
    )
  }

  implicit val schema: Schema[InnerRisk] = Schema.derived

  import zio.blocks.schema._

  private def territoryLen(
      index: Int
  ): Optional[List[TerritoryOccupationInfo], TerritoryOccupationInfo] = {
    Optional.at(
      Schema[List[TerritoryOccupationInfo]].reflect.asSequence.get,
      index = index
    )
  }

}
