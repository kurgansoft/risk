package risk.backend

import gbge.backend.models.Player
import gbge.backend.{BackendGame, Failure, GeneralFailure, UnauthorizedFailure}
import gbge.shared.GameState
import gbge.shared.actions.{Action, GameAction, UnassignRole}
import risk.shared.ClientRisk
import risk.shared.abstract0.*
import zio.{IO, ZIO}

import scala.language.implicitConversions

case class Risk(
    override val noOfPlayers: Int,
    override val innerRisk: Option[InnerRisk] = None,
    override val activeRoles: Set[RiskRole] = Set.empty
) extends AbstractRisk
    with BackendGame[RiskRole, RiskAction, ClientRisk] {

  implicit def convert(
      risk: Risk
  ): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = Right(
    (risk, ZIO.none)
  )

  override def registerRole(
      gameRole: RiskRole
  ): Either[Failure, BackendGame[RiskRole, RiskAction, ClientRisk]] = {
    if (noOfPlayers == activeRoles.size)
      Left(
        GeneralFailure(
          s"Currently only [$noOfPlayers] roles are supported, cannot register a new one!"
        )
      )
    else
      super.registerRole(gameRole)
  }

  override def setActiveRoles(
      roles: Set[RiskRole]
  ): BackendGame[RiskRole, RiskAction, ClientRisk] =
    this.copy(activeRoles = roles)

  override def increaseRoomSize()
      : Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    val newPlayerNumber = Math.min(noOfPlayers + 1, maxPlayerNumber)
    if (state == GameState.NOT_STARTED) {
      if (newPlayerNumber == noOfPlayers)
        Left(GeneralFailure("Cannot increase room size, already @ max."))
      else
        this.copy(noOfPlayers = newPlayerNumber)
    } else {
      Left(
        GeneralFailure(
          "Cannot change the no. of players; game has already started."
        )
      )
    }
  }

  override def decreaseRoomSize()
      : Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    val newPlayerNumber = Math.max(minPlayerNumber, noOfPlayers - 1)
    if (state == GameState.NOT_STARTED) {
      if (newPlayerNumber == noOfPlayers)
        Left(GeneralFailure("Cannot decrease room size, already @ min."))
      else if (newPlayerNumber >= activeRoles.size)
        Right(
          this.copy(noOfPlayers = newPlayerNumber),
          ZIO.none
        )
      else {
        val roleIdToUnassign = activeRoles.toList.maxBy(_.roleId).roleId
        Right(
          this.copy(
            noOfPlayers = newPlayerNumber,
            activeRoles =
              activeRoles - RiskRole.getRoleFromRoleId(roleIdToUnassign)
          ),
          ZIO.some(UnassignRole(roleIdToUnassign))
        )
      }
    } else {
      Left(
        GeneralFailure(
          "Cannot change the no. of players; game has already started."
        )
      )
    }
  }

  override def reduce(
      gameAction: GameAction,
      invoker: Player
  ): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    gameAction match {
      case riskAction: RiskAction =>
        reduce0(riskAction, invoker.role, invoker.isAdmin)
      case _ =>
        Left(GeneralFailure("Improper action"))
    }
  }

  def reduce0(
      riskAction: RiskAction,
      invokerRole: Option[Int],
      isAdmin: Boolean = false
  ): Either[Failure, (Risk, IO[Nothing, Option[Action]])] = {
    if (riskAction.adminOnly && !isAdmin) {
      Left(UnauthorizedFailure("Admin-only action."))
    } else {
      riskAction match {
        case Init =>
          if (
            activeRoles.size == noOfPlayers && state == GameState.NOT_STARTED
          ) {
            Right(this, RiskEffects.dtbp)
          } else {
            this
          }
        case DivideTerritoriesBetweenPlayers(seed) =>
          val inner = InnerRisk.generate(activeRoles, seed)
          copy(innerRisk = Some(inner))
        case AddUnitToTerritory(_) | DeployUnitToTerritory(_) |
            RetractUnitFromTerritory(_) | CommitDeployment |
            LaunchCampaign(_, _) | MoveInToConqueredTerritory(_) |
            AbandonCurrentCampaign | Skip | _: ManeuverTroops |
            _: CampaignAction | _: Toggle =>
          if (innerRisk.isEmpty) {
            Left(GeneralFailure("Inner game is missing"))
          } else {
            val role =
              invokerRole.flatMap(id => RiskRole.allRole.find(_.roleId == id))
            if (role.isEmpty) {
              Left(UnauthorizedFailure("Authentication required."))
            } else
              innerRisk.get.reduce(riskAction, role.get) match {
                case Left(failure)   => Left(failure)
                case Right(newInner) =>
                  val newRisk = this.copy(innerRisk = Some(newInner))
                  val effect: IO[Nothing, Option[Action]] = riskAction match {
                    case Attack(_) =>
                      val campaign = newInner.campaign.get
                      val destinationTerritory =
                        newInner.territories(campaign.destination)
                      val destinationPlayer =
                        newInner.players(destinationTerritory.role)
                      if (
                        destinationPlayer.defendWithOneAutomaticallyIfOnlyOneUnitIsPresent && destinationTerritory.noOfUnits == 1
                      )
                        ZIO.some(Defend(1))
                      else
                        ZIO.none
                    case Defend(_) =>
                      val campaign = newInner.campaign.get
                      val sourceTerritory =
                        newInner.territories(campaign.source)
                      val sourcePlayer = newInner.players(sourceTerritory.role)
                      val destinationTerritory =
                        newInner.territories(campaign.destination)
                      val destinationPlayer =
                        newInner.players(destinationTerritory.role)
                      if (sourcePlayer.rollAttackerDieAutomatically)
                        ZIO.some(RollDiceForAttacker)
                      else if (destinationPlayer.rollDefenderDieAutomatically)
                        ZIO.some(RollDiceForDefender)
                      else
                        ZIO.none
                    case RollDiceForAttacker =>
                      val campaign = newInner.campaign.get
                      RiskEffects.attackerDiceEffect(
                        campaign.battle.noOfAttackers
                      )
                    case RollDiceForDefender =>
                      val campaign = newInner.campaign.get
                      RiskEffects.defenderDiceEffect(
                        campaign.battle.noOfDefenders
                      )
                    case DiceResultForAttacker(_) =>
                      val campaign = newInner.campaign.get
                      val destinationTerritory =
                        newInner.territories(campaign.destination)
                      val destinationPlayer =
                        newInner.players(destinationTerritory.role)
                      if (
                        destinationPlayer.rollDefenderDieAutomatically && campaign.battle.defenderDie.isEmpty
                      )
                        ZIO.some(RollDiceForDefender)
                      else
                        ZIO.none
                    case _ => ZIO.none
                  }
                  Right((newRisk, effect))
              }
          }
        case _ =>
          Left(GeneralFailure("Action not implemented"))
      }
    }
  }

  override def toFrontendGame(role: Option[Int]): ClientRisk = ClientRisk(
    noOfPlayers,
    innerRisk.map(
      _.toClientInnerGame(
        role.flatMap(id => RiskRole.allRole.find(_.roleId == id))
      )
    ),
    activeRoles
  )
}
