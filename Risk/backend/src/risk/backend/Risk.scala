package risk.backend

import gbge.backend._
import gbge.shared.actions.{Action, GameAction, NaturalLink, UnassignRole}
import gbge.shared.{DecodeCapable, GameState, IN_PROGRESS, NOT_STARTED}
import risk.shared.abstract0._
import risk.shared.ClientRisk
import zio.ZIO

import scala.util.Random

object RiskEffects {
  import zio._
  val dtbp: Universe => UIO[List[Action]] = _ => {
    val seed = Random.nextLong().abs
    ZIO.succeed(List(DivideTerritoriesBetweenPlayers(seed)))
  }

  val defenderDiceEffect: Universe => UIO[List[Action]] = universe => ZIO.effectTotal {
    val risk = universe.game.get.asInstanceOf[Risk]
    val innerRisk = risk.innerRisk.get
    val nod = innerRisk.campaign.get.battle.noOfDefenders
    val diceResult = (for (_ <- 1 to nod)
      yield Random.nextInt(6) +1).toList
    Thread.sleep(2000)
    List(DiceResultForDefender(diceResult))
  }

  val attackerDiceEffect: Universe => UIO[List[Action]] = universe => ZIO.effectTotal {
    val risk = universe.game.get.asInstanceOf[Risk]
    val innerRisk = risk.innerRisk.get
    val noa = innerRisk.campaign.get.battle.noOfAttackers
    val diceResult = (for (_ <- 1 to noa)
      yield Random.nextInt(6) +1).toList
    Thread.sleep(2000)
    List(DiceResultForAttacker(diceResult))
  }
}
case class Risk(
                 override val noOfPlayers: Int,
                 override val colorMap: ColorMap = ColorMap(),
                 override val state: GameState = NOT_STARTED,
                 override val innerRisk: Option[InnerRisk] = None
               ) extends AbstractRisk with BackendGame[ClientRisk] {

  implicit def i1(risk: Risk): (Risk, UniverseResult) = (risk, OK)
  implicit def i2(universeResult: UniverseResult): (Risk, UniverseResult) = (this, universeResult)

  override def increaseRoomSize(): (BackendGame[ClientRisk], UniverseResult) = {
    val newPlayerNumber = Math.min(noOfPlayers + 1, maxPlayerNumber)
    if (state == NOT_STARTED) {
      if (newPlayerNumber == noOfPlayers)
        (this, GeneralFailure("Cannot increase room size, already @ max."))
      else
        (this.copy(noOfPlayers = newPlayerNumber), OK)
    } else {
      (this, GeneralFailure("Cannot change the no. of players; game has already started."))
    }
  }

  override def decreaseRoomSize(): (BackendGame[ClientRisk], UniverseResult) = {
    val newPlayerNumber = Math.max(minPlayerNumber, noOfPlayers-1)
    if (state == NOT_STARTED) {
      if (newPlayerNumber == noOfPlayers)
        (this, GeneralFailure("Cannot decrease room size, already @ min."))
      else {
        val disappearedRole = newPlayerNumber + 1
        val newColorMap = if (colorMap.colorOfRole(disappearedRole).isDefined)
         colorMap.removeColor(colorMap.colorOfRole(disappearedRole).get)
        else
          colorMap
        (this.copy(noOfPlayers = newPlayerNumber, colorMap = newColorMap), ExecuteEffect(_ => ZIO.succeed(List(UnassignRole(disappearedRole)))))
      }
    } else {
      (this, GeneralFailure("Cannot change the no. of players; game has already started."))
    }
  }

  override def reduce(gameAction: GameAction, invoker: Option[Player]): (BackendGame[ClientRisk], UniverseResult) = {
    if (gameAction.isInstanceOf[RiskAction]) {
      val riskAction = gameAction.asInstanceOf[RiskAction]
      reduce0(riskAction, invoker.flatMap(_.role), invoker.exists(_.isAdmin))
    } else {
      (this, GeneralFailure("Improper action"))
    }
  }

  def reduce0(riskAction: RiskAction, invokerRole: Option[Int], isAdmin: Boolean = false): (Risk, UniverseResult) = {
    if (riskAction.adminOnly && !isAdmin) {
      (this, UnauthorizedFailure("Admin-only action."))
    } else {
      riskAction match {
        case PickColor(roleId, color) => {
          if (state == IN_PROGRESS) {
            GeneralFailure("Can't pick color, game is already in progress.")
          } else if (invokerRole.isEmpty) {
            UnauthorizedFailure("Authentication required.")
          } else if (!isAdmin && !invokerRole.contains(roleId)) {
            UnauthorizedFailure("Only the admin or the player can assign a color to a role.")
          } else {
            if (colorMap.roleOfColor(color).isDefined) {
              (this, GeneralFailure("Color is taken already"))
            } else {
              copy(colorMap = colorMap.addOrUpdateColor(color, roleId))
            }
          }
        }
        case DropColor(roleId) => {
          if (state == IN_PROGRESS) {
            GeneralFailure("Can't drop color, game is already in progress.")
          } else if (invokerRole.isEmpty) {
            UnauthorizedFailure("Authentication required.")
          } else if (!isAdmin && !invokerRole.contains(roleId)) {
            UnauthorizedFailure("Only the admin or the player can assign a color to a role.")
          } else {
            val color = colorMap.colorOfRole(roleId)
            if (color.isEmpty) {
              (this, GeneralFailure("No color to drop"))
            } else {
              copy(colorMap = colorMap.removeColor(color.get))
            }
          }
        }
        case Init => {
          if (colorMap.size == noOfPlayers && state == NOT_STARTED) {
            (copy(state = IN_PROGRESS), ExecuteEffect(RiskEffects.dtbp))
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
          val newInnerRisk = innerRisk.map(_.reduce(riskAction, invokerRole, isAdmin))
          (this.copy(innerRisk = newInnerRisk.map(_._1)), newInnerRisk.map(_._2).getOrElse(GeneralFailure("Inner game is missing")))
        }
        case _ => {
          (this, GeneralFailure("Action not implemented"))
        }
      }
    }
  }

  override def toFrontendGame(role: Option[Int]): ClientRisk = ClientRisk(noOfPlayers, colorMap, state, innerRisk.map(_.toClientInnerGame(role)))
}

object Risk extends Startable {
  override def start(noOfPlayers: Int): (Risk, Option[Action]) = {
    val z = noOfPlayers match {
      case x if x <=3 => 3
      case 4 => 4
      case _ => 5
    }
    (Risk(z), Some(NaturalLink((1 to z).toList)))
  }

  override val frontendGame: DecodeCapable = ClientRisk
  override val name: String = "Risk"
}