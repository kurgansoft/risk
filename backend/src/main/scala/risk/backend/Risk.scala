package risk.backend

import gbge.backend.models.Player
import gbge.backend.{BackendGame, Failure, GeneralFailure, UnauthorizedFailure}
import gbge.shared.GameState
import gbge.shared.actions.{Action, GameAction, UnassignRole}
import risk.shared.ClientRisk
import risk.shared.abstract0._
import zio.{IO, ZIO}

import scala.language.implicitConversions

case class Risk(
                 override val noOfPlayers: Int,
                 override val colorMap: ColorMap = ColorMap(),
                 override val state: GameState = GameState.NOT_STARTED,
                 override val innerRisk: Option[InnerRisk] = None
               ) extends AbstractRisk with BackendGame[RiskAction, ClientRisk] {

  implicit def convert(risk: Risk): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = Right((risk, ZIO.none))

  override def increaseRoomSize(): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    val newPlayerNumber = Math.min(noOfPlayers + 1, maxPlayerNumber)
    if (state == GameState.NOT_STARTED) {
      if (newPlayerNumber == noOfPlayers)
        Left(GeneralFailure("Cannot increase room size, already @ max."))
      else
        this.copy(noOfPlayers = newPlayerNumber)
    } else {
      Left(GeneralFailure("Cannot change the no. of players; game has already started."))
    }
  }

  override def decreaseRoomSize(): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    val newPlayerNumber = Math.max(minPlayerNumber, noOfPlayers-1)
    if (state == GameState.NOT_STARTED) {
      if (newPlayerNumber == noOfPlayers)
        Left(GeneralFailure("Cannot decrease room size, already @ min."))
      else {
        val disappearedRole = newPlayerNumber + 1
        val newColorMap = if (colorMap.colorOfRole(disappearedRole).isDefined)
         colorMap.removeColor(colorMap.colorOfRole(disappearedRole).get)
        else
          colorMap
        Right((this.copy(noOfPlayers = newPlayerNumber, colorMap = newColorMap), ZIO.some(UnassignRole(disappearedRole))))
      }
    } else {
      Left(GeneralFailure("Cannot change the no. of players; game has already started."))
    }
  }

  override def reduce(gameAction: GameAction, invoker: Player): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    gameAction match {
      case riskAction: RiskAction =>
        reduce0(riskAction, invoker.role, invoker.isAdmin)
      case _ =>
        Left(GeneralFailure("Improper action"))
    }
  }

  def reduce0(riskAction: RiskAction, invokerRole: Option[Int], isAdmin: Boolean = false): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    if (riskAction.adminOnly && !isAdmin) {
      Left(UnauthorizedFailure("Admin-only action."))
    } else {
      riskAction match {
        case PickColor(roleId, color) => {
          if (state == GameState.IN_PROGRESS) {
            Left(GeneralFailure("Can't pick color, game is already in progress."))
          } else if (invokerRole.isEmpty) {
            Left(UnauthorizedFailure("Authentication required."))
          } else if (!isAdmin && !invokerRole.contains(roleId)) {
            Left(UnauthorizedFailure("Only the admin or the player can assign a color to a role."))
          } else {
            if (colorMap.roleOfColor(color).isDefined) {
              Left(GeneralFailure("Color is taken already"))
            } else {
              copy(colorMap = colorMap.addOrUpdateColor(color, roleId))
            }
          }
        }
        case DropColor(roleId) => {
          if (state == GameState.IN_PROGRESS) {
            Left(GeneralFailure("Can't drop color, game is already in progress."))
          } else if (invokerRole.isEmpty) {
            Left(UnauthorizedFailure("Authentication required."))
          } else if (!isAdmin && !invokerRole.contains(roleId)) {
            Left(UnauthorizedFailure("Only the admin or the player can assign a color to a role."))
          } else {
            val color = colorMap.colorOfRole(roleId)
            if (color.isEmpty) {
              Left(GeneralFailure("No color to drop"))
            } else {
              copy(colorMap = colorMap.removeColor(color.get))
            }
          }
        }
        case Init => {
          if (colorMap.size == noOfPlayers && state == GameState.NOT_STARTED) {
            Right((copy(state = GameState.IN_PROGRESS), RiskEffects.dtbp))
          } else {
            this
          }
        }
        case DivideTerritoriesBetweenPlayers(seed) => {
          val inner = InnerRisk.generate(colorMap, seed)
          copy(innerRisk = Some(inner))
        }
        case AddUnitToTerritory(_) | DeployUnitToTerritory(_) | RetractUnitFromTerritory(_) | CommitDeployment |
             LaunchCampaign(_,_) | MoveInToConqueredTerritory(_) | AbandonCurrentCampaign | Skip |
             _ :ManeuverTroops | _:CampaignAction | _: Toggle => {
          if (innerRisk.isEmpty) {
            Left(GeneralFailure("Inner game is missing"))
          } else {
            innerRisk.get.reduce(riskAction, invokerRole, isAdmin) match {
              case Left(failure) => Left(failure)
              case Right(newInner) =>
                val newRisk = this.copy(innerRisk = Some(newInner))
                val effect: IO[Nothing, Option[Action]] = riskAction match {
                  case Attack(_) => {
                    val campaign = newInner.campaign.get
                    val destinationTerritory = newInner.territories(campaign.destination)
                    val destinationPlayer = newInner.players.find(_.role == destinationTerritory.roleId).get
                    if (destinationPlayer.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent && destinationTerritory.noOfUnits == 1)
                      ZIO.some(Defend(1))
                    else
                      ZIO.none
                  }
                  case Defend(_) => {
                    val campaign = newInner.campaign.get
                    val sourceTerritory = newInner.territories(campaign.source)
                    val sourcePlayer = newInner.players.find(_.role == sourceTerritory.roleId).get
                    val destinationTerritory = newInner.territories(campaign.destination)
                    val destinationPlayer = newInner.players.find(_.role == destinationTerritory.roleId).get
                    if (sourcePlayer.rollAttackerDieAutomatically)
                      ZIO.some(RollDiceForAttacker)
                    else if (destinationPlayer.rollDefenderDieAutomatically)
                      ZIO.some(RollDiceForDefender)
                    else
                      ZIO.none
                  }
                  case RollDiceForAttacker => {
                    val campaign = newInner.campaign.get
                    RiskEffects.attackerDiceEffect(campaign.battle.noOfAttackers)
                  }
                  case RollDiceForDefender => {
                    val campaign = newInner.campaign.get
                    RiskEffects.defenderDiceEffect(campaign.battle.noOfDefenders)
                  }
                  case DiceResultForAttacker(_) => {
                    val campaign = newInner.campaign.get
                    val destinationTerritory = newInner.territories(campaign.destination)
                    val destinationPlayer = newInner.players.find(_.role == destinationTerritory.roleId).get
                    if (destinationPlayer.rollDefenderDieAutomatically && campaign.battle.defenderDie.isEmpty)
                      ZIO.some(RollDiceForDefender)
                    else
                      ZIO.none
                  }
                  case _ => ZIO.none
                }
                Right((newRisk, effect))
            }
          }
        }
        case _ => {
          Left(GeneralFailure("Action not implemented"))
        }
      }
    }
  }

  override def toFrontendGame(role: Option[Int]): ClientRisk = ClientRisk(noOfPlayers, colorMap, state, innerRisk.map(_.toClientInnerGame(role)))
}
