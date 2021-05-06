package risk.ui

import gbge.client.{ClientEvent, ClientEventHandler, ClientResult, OK, PrepareRestActionWithToken}
import gbge.shared.{FrontendPlayer, FrontendUniverse, IN_PROGRESS}
import gbge.ui.eps.player.{ClientState, ScreenEvent}
import gbge.ui.state.OfflineState
import japgolly.scalajs.react.vdom.TagOf
import org.scalajs.dom.html.Div
import risk.shared.{ClientRisk, ClientRiskPlayer}
import risk.shared.abstract0.{AbandonCurrentCampaign, AddUnitToTerritory, AddingUnits, Attack, BattleAndManeuver, CommitDeployment, Defend, DeployUnitToTerritory, DeployingUnits, Init, LaunchCampaign, ManeuverTroops, MoveInToConqueredTerritory, RetractUnitFromTerritory, RollDiceForAttacker, RollDiceForDefender, Skip, ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent, ToggleRollAttackerDieAutomatically, ToggleRollDefenderDieAutomatically}

abstract sealed class OfflineRiskEvent extends ScreenEvent
case class TerritoryClicked(index: Int) extends OfflineRiskEvent
case class SelectOfflineActionMode(offlineActionMode: OfflineActionMode) extends OfflineRiskEvent
case object CancelOfflineActionMode extends OfflineRiskEvent
case class SwitchToTab(riskTab: RiskTab) extends OfflineRiskEvent
case object ClientToggleRollAttackerDieAutomatically extends OfflineRiskEvent
case object ClientToggleRollDefenderDieAutomatically extends OfflineRiskEvent
case object ClientToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent extends OfflineRiskEvent

case object ClientInit extends OfflineRiskEvent
case object ClientCommit extends OfflineRiskEvent
case object ClientSkip extends OfflineRiskEvent
case object ClientLaunch extends OfflineRiskEvent
case class ClientAttack(noOfAttackers: Int) extends OfflineRiskEvent
case class ClientDefend(noOfDefenders: Int) extends OfflineRiskEvent
case object ClientRollDiceForAttacker extends OfflineRiskEvent
case object ClientRollDiceForDefender extends OfflineRiskEvent
case object ClientAbandonCurrentCampaign extends OfflineRiskEvent
case class ClientMoveInToConqueredTerritory(noOfTroopsToMoveIn: Int) extends OfflineRiskEvent
case class ClientManeuverTroops(from: Int, to: Int, noOfTroops: Int) extends OfflineRiskEvent
case object ClientIncreaseProposedNumberOfPlayers extends OfflineRiskEvent
case object ClientDecreaseProposedNumberOfPlayers extends OfflineRiskEvent

abstract sealed class OfflineActionMode
case object DeployUnitMode extends OfflineActionMode
case object RetractUnitMode extends OfflineActionMode
case class AttackMode(source: Option[Int] = None, destination: Option[Int] = None) extends OfflineActionMode
case class ManeuverMode(source: Option[Int] = None, destination: Option[Int] = None, noOfTroops: Int = 1) extends OfflineActionMode

abstract sealed class RiskTab
case object MapTab extends RiskTab
case object CampaignTab extends RiskTab
case object InventoryTab extends RiskTab

case class OfflineRiskState(actionMode: Option[OfflineActionMode] = None, tab: RiskTab = MapTab) extends OfflineState {
  override def handleScreenEvent(screenEvent: ScreenEvent, fu: Option[FrontendUniverse], you: Option[FrontendPlayer]): (OfflineState, ClientResult) = {
    screenEvent match {
      case offlineRiskEvent: OfflineRiskEvent => reduce0(offlineRiskEvent, fu, you)
      case _ => this
    }
  }

  def reduce0(event: OfflineRiskEvent, fu: Option[FrontendUniverse], you: Option[FrontendPlayer]): (OfflineState, ClientResult) = {
    val clientRisk = fu.get.game.get.asInstanceOf[ClientRisk]
    event match {
      case ClientToggleRollAttackerDieAutomatically => {
        if (you.isDefined && you.get.role.isDefined)
          (this, PrepareRestActionWithToken(ToggleRollAttackerDieAutomatically(you.get.role.get)))
        else
          this
      }
      case ClientToggleRollDefenderDieAutomatically => {
        if (you.isDefined && you.get.role.isDefined)
          (this, PrepareRestActionWithToken(ToggleRollDefenderDieAutomatically(you.get.role.get)))
        else
          this
      }
      case ClientToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent => {
        if (you.isDefined && you.get.role.isDefined)
          (this, PrepareRestActionWithToken(ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent(you.get.role.get)))
        else
          this
      }
      case TerritoryClicked(index) => {
        if (clientRisk.state == IN_PROGRESS && clientRisk.innerRisk.isDefined) {
          val innerRisk = clientRisk.innerRisk.get
          innerRisk.phase match {
            case AddingUnits => {
              if (you.flatMap(_.role).contains(innerRisk.currentPlayer) && innerRisk.territories(index).roleId == innerRisk.currentPlayer) {
                (this, PrepareRestActionWithToken(AddUnitToTerritory(index)))
              } else {
                (this, OK)
              }
            }
            case DeployingUnits => {
              if (you.flatMap(_.role).contains(innerRisk.currentPlayer)) {
                if (actionMode.contains(DeployUnitMode) &&
                    innerRisk.territories(index).roleId == innerRisk.currentPlayer &&
                    innerRisk.getCurrentPlayer.troops != 0) {
                  (this, PrepareRestActionWithToken(DeployUnitToTerritory(index)))
                } else if (actionMode.contains(RetractUnitMode) &&
                  innerRisk.territories(index).roleId == innerRisk.currentPlayer &&
                  innerRisk.territories(index).noOfProposedUnits > 0) {
                  (this, PrepareRestActionWithToken(RetractUnitFromTerritory(index)))
                } else {
                  (this, OK)
                }
              } else {
                (this, OK)
              }
            }
            case BattleAndManeuver => {
              if (you.flatMap(_.role).contains(innerRisk.currentPlayer)) {
                actionMode match {
                  case Some(AttackMode(source, destination)) => {
                    val territory = innerRisk.territories(index)
                    if (source.isEmpty && territory.roleId == innerRisk.currentPlayer && territory.noOfUnits >= 2) {
                      (this.copy(actionMode = Some(AttackMode(Some(index), None))), OK)
                    } else if (source.isDefined && destination.isEmpty &&
                               innerRisk.areTheyNeighbours(source.get, index) &&
                               innerRisk.territories(index).roleId != innerRisk.currentPlayer) {
                      (this.copy(actionMode = Some(AttackMode(source, Some(index)))), OK)
                    } else {
                      (this, OK)
                    }
                  }
                  case Some(ManeuverMode(from, to, noOfTroops)) => {
                    val territory = innerRisk.territories(index)
                    val newManeuverState =
                      if (territory.roleId != innerRisk.currentPlayer) {
                        ManeuverMode(from, to, noOfTroops)
                      } else if (from.isEmpty && territory.noOfUnits >=2) {
                        ManeuverMode(Some(index), to, noOfTroops)
                      } else if (from.isDefined && to.isEmpty && innerRisk.areTheyConnected(from.get, index)) {
                        ManeuverMode(from, Some(index), noOfTroops)
                      } else {
                        ManeuverMode(from, to, noOfTroops)
                      }
                    (this.copy(actionMode = Some(newManeuverState)), OK)
                  }
                  case _ => (this, OK)
                }
              } else {
                (this, OK)
              }
            }
            case _ => (this, OK)
          }
        } else {
          (this, OK)
        }
      }
      case SelectOfflineActionMode(actionMode) => {
        (this.copy(actionMode = Some(actionMode)), OK)
      }
      case CancelOfflineActionMode => (this.copy(actionMode = None), OK)
      case ClientCommit => {
        (this.copy(actionMode = None), PrepareRestActionWithToken(CommitDeployment))
      }
      case ClientSkip => {
        (this, PrepareRestActionWithToken(Skip))
      }
      case ClientLaunch => {
        actionMode match {
          case Some(AttackMode(source, destination)) => {
            if (source.isDefined && destination.isDefined) {
              (this, PrepareRestActionWithToken(LaunchCampaign(source.get, destination.get)))
            } else {
              (this, OK)
            }
          }
          case _ => (this, OK)
        }
      }
      case SwitchToTab(tab) => {
        (this.copy(tab = tab), OK)
      }
      case ClientInit => {
        (this, PrepareRestActionWithToken(Init))
      }
      case ClientAttack(noOfAttackers) => {
        //check stuff
        (this, PrepareRestActionWithToken(Attack(noOfAttackers)))
      }
      case ClientDefend(noOfDefenders) => {
        //check stuff
        (this, PrepareRestActionWithToken(Defend(noOfDefenders)))
      }
      case ClientRollDiceForAttacker => {
        //check stuff
        (this, PrepareRestActionWithToken(RollDiceForAttacker))
      }
      case ClientRollDiceForDefender => {
        //check stuff
        (this, PrepareRestActionWithToken(RollDiceForDefender))
      }
      case ClientAbandonCurrentCampaign => {
        //check stuff
        (this, PrepareRestActionWithToken(AbandonCurrentCampaign))
      }
      case ClientMoveInToConqueredTerritory(noOfTroopsToMoveIn) => {
        //check stuff
        (this.copy(actionMode = None), PrepareRestActionWithToken(MoveInToConqueredTerritory(noOfTroopsToMoveIn)))
      }
      case ClientManeuverTroops(from, to, noOfTroops) => {
        //check stuff
        (this.copy(actionMode = None), PrepareRestActionWithToken(ManeuverTroops(from, to, noOfTroops)))
      }
      case ClientDecreaseProposedNumberOfPlayers => {
        //check stuff
        (this, PrepareRestActionWithToken(gbge.shared.actions.DecreaseProposedNumberOfPlayers))
      }
      case ClientIncreaseProposedNumberOfPlayers => {
        //check stuff
        (this, PrepareRestActionWithToken(gbge.shared.actions.IncreaseProposedNumberOfPlayers))
      }
      case _ => (this, OK)
    }
  }
}