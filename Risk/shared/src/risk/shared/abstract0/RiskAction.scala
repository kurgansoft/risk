package risk.shared.abstract0

import gbge.shared.actions.GameAction
import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class RiskAction extends GameAction {
  override def serialize(): String = upickle.default.write[RiskAction](this)
}

object RiskAction {
  implicit def rw: RW[RiskAction] = macroRW
}

case class DropColor(roleId: Int) extends RiskAction

object DropColor {
  implicit def rw: RW[DropColor] = macroRW
}

case class PickColor(roleId: Int, color: RiskColor) extends RiskAction

object PickColor {
  implicit def rw: RW[PickColor] = macroRW
}

abstract sealed class Toggle(val roleId: Int) extends RiskAction

object Toggle {
  implicit def rw: RW[Toggle] = macroRW
}

case class ToggleRollAttackerDieAutomatically(override val roleId: Int) extends Toggle(roleId)
object ToggleRollAttackerDieAutomatically {
  implicit def rw: RW[ToggleRollAttackerDieAutomatically] = macroRW
}

case class ToggleRollDefenderDieAutomatically(override val roleId: Int) extends Toggle(roleId)
object ToggleRollDefenderDieAutomatically {
  implicit def rw: RW[ToggleRollDefenderDieAutomatically] = macroRW
}

case class ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent(override val roleId: Int) extends Toggle(roleId)
object ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent {
  implicit def rw: RW[ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent] = macroRW
}

case object Init extends RiskAction {
  override val adminOnly: Boolean = true
}

case class DivideTerritoriesBetweenPlayers(seed: Long) extends RiskAction {
  override val systemOnly: Boolean = true
}

object DivideTerritoriesBetweenPlayers {
  implicit def rw: RW[DivideTerritoriesBetweenPlayers] = macroRW
}

case class AddUnitToTerritory(targetTerritory: Int) extends RiskAction

object AddUnitToTerritory {
  implicit def rw: RW[AddUnitToTerritory] = macroRW
}

case class DeployUnitToTerritory(targetTerritory: Int) extends RiskAction

object DeployUnitToTerritory {
  implicit def rw: RW[DeployUnitToTerritory] = macroRW
}

case class RetractUnitFromTerritory(targetTerritory: Int) extends RiskAction

object RetractUnitFromTerritory {
  implicit def rw: RW[RetractUnitFromTerritory] = macroRW
}

case object CommitDeployment extends RiskAction

case class LaunchCampaign(source: Int, destination: Int) extends RiskAction {
}

object LaunchCampaign {
  implicit def rw: RW[LaunchCampaign] = macroRW
}

abstract sealed class CampaignAction extends RiskAction

object CampaignAction {
  implicit def rw: RW[CampaignAction] = macroRW
}

case class Attack(noOfAttackers: Int) extends CampaignAction {
  assert(noOfAttackers >= 1 && noOfAttackers <= 3)
}

object Attack {
  implicit def rw: RW[Attack] = macroRW
}

case class Defend(noOfDefenders: Int) extends CampaignAction {
  assert(noOfDefenders == 1 || noOfDefenders == 2)
}

object Defend {
  implicit def rw: RW[Defend] = macroRW
}

case object RollDiceForAttacker extends CampaignAction
case object RollDiceForDefender extends CampaignAction

case class DiceResultForAttacker(dices: List[Int]) extends CampaignAction {
  assert(dices.forall(dice => dice >= 1 && dice <= 6))
  assert(dices.nonEmpty && dices.size <=3)

  override val systemOnly: Boolean = true
}

object DiceResultForAttacker {
  implicit def rw: RW[DiceResultForAttacker] = macroRW
}

case class DiceResultForDefender(dices: List[Int]) extends CampaignAction {
  assert(dices.forall(dice => dice >= 1 && dice <= 6))
  assert(dices.nonEmpty && dices.size <=3)

  override val systemOnly: Boolean = true
}

object DiceResultForDefender {
  implicit def rw: RW[DiceResultForDefender] = macroRW
}

case class MoveInToConqueredTerritory(noOfTroops: Int) extends RiskAction

object MoveInToConqueredTerritory {
  implicit def rw: RW[MoveInToConqueredTerritory] = macroRW
}

case class ManeuverTroops(from: Int, to: Int, noOfTroops: Int) extends RiskAction {
  assert(from >= 0)
  assert(to >= 0)
  assert(noOfTroops >= 1)
}

object ManeuverTroops {
  implicit def rw: RW[ManeuverTroops] = macroRW
}

case object AbandonCurrentCampaign extends RiskAction

case object Skip extends RiskAction

