package risk.ui

import gbge.client.events_and_effects.{DispatchActionWithToken, ScreenEvent}
import gbge.shared.{FrontendUniverse, GameState}
import gbge.ui.state.OfflineState
import risk.shared.ClientRisk
import risk.shared.abstract0.*
import risk.shared.abstract0.GamePhase.*
import uiglue.Event
import zio.{UIO, ZIO}

import scala.language.implicitConversions

abstract sealed class OfflineRiskEvent extends ScreenEvent
case class TerritoryClicked(index: Int) extends OfflineRiskEvent
case class SelectOfflineActionMode(offlineActionMode: OfflineActionMode)
    extends OfflineRiskEvent
case object CancelOfflineActionMode extends OfflineRiskEvent
case class SwitchToTab(riskTab: RiskTab) extends OfflineRiskEvent
case object ClientToggleRollAttackerDieAutomatically extends OfflineRiskEvent
case object ClientToggleRollDefenderDieAutomatically extends OfflineRiskEvent
case object ClientToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent
    extends OfflineRiskEvent

case object ClientInit extends OfflineRiskEvent
case object ClientCommit extends OfflineRiskEvent
case object ClientSkip extends OfflineRiskEvent
case object ClientLaunch extends OfflineRiskEvent
case class ClientAttack(noOfAttackers: Int) extends OfflineRiskEvent
case class ClientDefend(noOfDefenders: Int) extends OfflineRiskEvent
case object ClientRollDiceForAttacker extends OfflineRiskEvent
case object ClientRollDiceForDefender extends OfflineRiskEvent
case object ClientAbandonCurrentCampaign extends OfflineRiskEvent
case class ClientMoveInToConqueredTerritory(noOfTroopsToMoveIn: Int)
    extends OfflineRiskEvent
case class ClientManeuverTroops(from: Int, to: Int, noOfTroops: Int)
    extends OfflineRiskEvent
case object ClientIncreaseProposedNumberOfPlayers extends OfflineRiskEvent
case object ClientDecreaseProposedNumberOfPlayers extends OfflineRiskEvent

abstract sealed class OfflineActionMode
case object DeployUnitMode extends OfflineActionMode
case object RetractUnitMode extends OfflineActionMode
case class AttackMode(
    source: Option[Int] = None,
    destination: Option[Int] = None
) extends OfflineActionMode
case class ManeuverMode(
    source: Option[Int] = None,
    destination: Option[Int] = None,
    noOfTroops: Int = 1
) extends OfflineActionMode

abstract sealed class RiskTab
case object MapTab extends RiskTab
case object CampaignTab extends RiskTab
case object InventoryTab extends RiskTab

case class OfflineRiskState(
    actionMode: Option[OfflineActionMode] = None,
    tab: RiskTab = MapTab
) extends OfflineState[Any] {

  implicit def convert(
      offlineRiskState: OfflineRiskState
  ): (OfflineRiskState, UIO[List[Event]]) =
    (offlineRiskState, ZIO.succeed(List.empty))

  override def handleScreenEvent(
      screenEvent: ScreenEvent,
      fu: Option[FrontendUniverse],
      playerId: Option[Int]
  ): (OfflineRiskState, UIO[List[Event]]) = {
    screenEvent match {
      case offlineRiskEvent: OfflineRiskEvent =>
        reduce0(offlineRiskEvent, fu, playerId)
      case _ => this
    }
  }

  private def reduce0(
      event: OfflineRiskEvent,
      fu: Option[FrontendUniverse],
      playerId: Option[Int]
  ): (OfflineRiskState, UIO[List[Event]]) = {
    val clientRisk = fu.get.game.get.asInstanceOf[ClientRisk]
    val you = playerId.flatMap(id => fu.flatMap(_.players.find(_.id == id)))
    event match {
      case ClientToggleRollAttackerDieAutomatically =>
        if (you.isDefined && you.get.role.isDefined)
          (
            this,
            ZIO.succeed(
              List(
                DispatchActionWithToken(
                  ToggleRollAttackerDieAutomatically(
                    RiskRole.getRoleFromRoleId(you.get.role.get)
                  )
                )
              )
            )
          )
        else
          this
      case ClientToggleRollDefenderDieAutomatically =>
        if (you.isDefined && you.get.role.isDefined)
          (
            this,
            ZIO.succeed(
              List(
                DispatchActionWithToken(
                  ToggleRollDefenderDieAutomatically(
                    RiskRole.getRoleFromRoleId(you.get.role.get)
                  )
                )
              )
            )
          )
        else
          this
      case ClientToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent =>
        if (you.isDefined && you.get.role.isDefined)
          (
            this,
            ZIO.succeed(
              List(
                DispatchActionWithToken(
                  ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent(
                    RiskRole.getRoleFromRoleId(you.get.role.get)
                  )
                )
              )
            )
          )
        else
          this
      case TerritoryClicked(index) =>
        if (clientRisk.innerRisk.isDefined) {
          val innerRisk = clientRisk.innerRisk.get
          innerRisk.phase match {
            case AddingUnits =>
              if (
                you
                  .flatMap(_.role)
                  .contains(
                    innerRisk.roleOfTheCurrentPlayer.roleId
                  ) && innerRisk
                  .territories(index)
                  .role == innerRisk.roleOfTheCurrentPlayer
              ) {
                (
                  this,
                  ZIO.succeed(
                    List(DispatchActionWithToken(AddUnitToTerritory(index)))
                  )
                )
              } else {
                this
              }
            case DeployingUnits =>
              if (
                you
                  .flatMap(_.role)
                  .contains(innerRisk.roleOfTheCurrentPlayer.roleId)
              ) {
                if (
                  actionMode.contains(DeployUnitMode) &&
                  innerRisk
                    .territories(index)
                    .role == innerRisk.roleOfTheCurrentPlayer &&
                  innerRisk.getCurrentPlayer.troops != 0
                ) {
                  (
                    this,
                    ZIO.succeed(
                      List(
                        DispatchActionWithToken(DeployUnitToTerritory(index))
                      )
                    )
                  )
                } else if (
                  actionMode.contains(RetractUnitMode) &&
                  innerRisk
                    .territories(index)
                    .role == innerRisk.roleOfTheCurrentPlayer &&
                  innerRisk.territories(index).noOfProposedUnits > 0
                ) {
                  (
                    this,
                    ZIO.succeed(
                      List(
                        DispatchActionWithToken(RetractUnitFromTerritory(index))
                      )
                    )
                  )
                } else {
                  this
                }
              } else {
                this
              }
            case BattleAndManeuver =>
              if (
                you
                  .flatMap(_.role)
                  .contains(innerRisk.roleOfTheCurrentPlayer.roleId)
              ) {
                actionMode match {
                  case Some(AttackMode(source, destination)) =>
                    val territory = innerRisk.territories(index)
                    if (
                      source.isEmpty && territory.role == innerRisk.roleOfTheCurrentPlayer && territory.noOfUnits >= 2
                    ) {
                      this.copy(actionMode =
                        Some(AttackMode(Some(index), None))
                      )
                    } else if (
                      source.isDefined && destination.isEmpty &&
                      innerRisk.areTheyNeighbours(source.get, index) &&
                      innerRisk
                        .territories(index)
                        .role != innerRisk.roleOfTheCurrentPlayer
                    ) {
                      this.copy(actionMode =
                        Some(AttackMode(source, Some(index)))
                      )
                    } else {
                      this
                    }
                  case Some(ManeuverMode(from, to, noOfTroops)) =>
                    val territory = innerRisk.territories(index)
                    val newManeuverState =
                      if (territory.role != innerRisk.roleOfTheCurrentPlayer) {
                        ManeuverMode(from, to, noOfTroops)
                      } else if (from.isEmpty && territory.noOfUnits >= 2) {
                        ManeuverMode(Some(index), to, noOfTroops)
                      } else if (
                        from.isDefined && to.isEmpty && innerRisk
                          .areTheyConnected(from.get, index)
                      ) {
                        ManeuverMode(from, Some(index), noOfTroops)
                      } else {
                        ManeuverMode(from, to, noOfTroops)
                      }
                    this.copy(actionMode = Some(newManeuverState))
                  case _ => this
                }
              } else {
                this
              }
            case _ => this
          }
        } else {
          this
        }
      case SelectOfflineActionMode(actionMode) =>
        this.copy(actionMode = Some(actionMode))
      case CancelOfflineActionMode => this.copy(actionMode = None)
      case ClientCommit            =>
        (
          this.copy(actionMode = None),
          ZIO.succeed(List(DispatchActionWithToken(CommitDeployment)))
        )
      case ClientSkip =>
        (this, ZIO.succeed(List(DispatchActionWithToken(Skip))))
      case ClientLaunch =>
        actionMode match {
          case Some(AttackMode(source, destination)) =>
            if (source.isDefined && destination.isDefined) {
              (
                this,
                ZIO.succeed(
                  List(
                    DispatchActionWithToken(
                      LaunchCampaign(source.get, destination.get)
                    )
                  )
                )
              )
            } else {
              this
            }
          case _ => this
        }
      case SwitchToTab(tab) =>
        this.copy(tab = tab)
      case ClientInit =>
        (this, ZIO.succeed(List(DispatchActionWithToken(Init))))
      case ClientAttack(noOfAttackers) =>
        // check stuff
        (
          this,
          ZIO.succeed(List(DispatchActionWithToken(Attack(noOfAttackers))))
        )
      case ClientDefend(noOfDefenders) =>
        // check stuff
        (
          this,
          ZIO.succeed(List(DispatchActionWithToken(Defend(noOfDefenders))))
        )
      case ClientRollDiceForAttacker =>
        // check stuff
        (this, ZIO.succeed(List(DispatchActionWithToken(RollDiceForAttacker))))
      case ClientRollDiceForDefender =>
        // check stuff
        (this, ZIO.succeed(List(DispatchActionWithToken(RollDiceForDefender))))
      case ClientAbandonCurrentCampaign =>
        // check stuff
        (
          this,
          ZIO.succeed(List(DispatchActionWithToken(AbandonCurrentCampaign)))
        )
      case ClientMoveInToConqueredTerritory(noOfTroopsToMoveIn) =>
        // check stuff
        (
          this.copy(actionMode = None),
          ZIO.succeed(
            List(
              DispatchActionWithToken(
                MoveInToConqueredTerritory(noOfTroopsToMoveIn)
              )
            )
          )
        )
      case ClientManeuverTroops(from, to, noOfTroops) =>
        // check stuff
        (
          this.copy(actionMode = None),
          ZIO.succeed(
            List(DispatchActionWithToken(ManeuverTroops(from, to, noOfTroops)))
          )
        )
      case ClientDecreaseProposedNumberOfPlayers =>
        // check stuff
        (
          this,
          ZIO.succeed(
            List(
              DispatchActionWithToken(
                gbge.shared.actions.DecreaseProposedNumberOfPlayers
              )
            )
          )
        )
      case ClientIncreaseProposedNumberOfPlayers =>
        // check stuff
        (
          this,
          ZIO.succeed(
            List(
              DispatchActionWithToken(
                gbge.shared.actions.IncreaseProposedNumberOfPlayers
              )
            )
          )
        )
    }
  }
}
