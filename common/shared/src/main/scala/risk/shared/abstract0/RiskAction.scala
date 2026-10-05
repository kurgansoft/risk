package risk.shared.abstract0

import gbge.shared.actions.GameAction
import zio.json.{EncoderOps, JsonCodec}
import zio.schema.{DeriveSchema, Schema}

sealed trait RiskAction extends GameAction {
  override def convertToJson(): String = this.toJson
}

object RiskAction {
  implicit val schema: Schema[RiskAction] = DeriveSchema.gen
  implicit val codec: JsonCodec[RiskAction] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}

case class DropColor(roleId: Int) extends RiskAction

case class PickColor(roleId: Int, color: RiskColor) extends RiskAction

abstract sealed class Toggle(val roleId: Int) extends RiskAction

case class ToggleRollAttackerDieAutomatically(override val roleId: Int) extends Toggle(roleId)

case class ToggleRollDefenderDieAutomatically(override val roleId: Int) extends Toggle(roleId)

case class ToggleDefendWithOneAutomaticallyIfOnlyOneUnitIsPresent(override val roleId: Int) extends Toggle(roleId)

case object Init extends RiskAction {
  override val adminOnly: Boolean = true
}

case class DivideTerritoriesBetweenPlayers(seed: Long) extends RiskAction {
  override val systemOnly: Boolean = true
}

case class AddUnitToTerritory(targetTerritory: Int) extends RiskAction

case class DeployUnitToTerritory(targetTerritory: Int) extends RiskAction

case class RetractUnitFromTerritory(targetTerritory: Int) extends RiskAction

case object CommitDeployment extends RiskAction

case class LaunchCampaign(source: Int, destination: Int) extends RiskAction

abstract sealed class CampaignAction extends RiskAction

case class Attack(noOfAttackers: Int) extends CampaignAction {
  assert(noOfAttackers >= 1 && noOfAttackers <= 3)
}

case class Defend(noOfDefenders: Int) extends CampaignAction {
  assert(noOfDefenders == 1 || noOfDefenders == 2)
}

case object RollDiceForAttacker extends CampaignAction
case object RollDiceForDefender extends CampaignAction

case class DiceResultForAttacker(dices: List[Int]) extends CampaignAction {
  assert(dices.forall(dice => dice >= 1 && dice <= 6))
  assert(dices.nonEmpty && dices.size <=3)

  override val systemOnly: Boolean = true
}

case class DiceResultForDefender(dices: List[Int]) extends CampaignAction {
  assert(dices.forall(dice => dice >= 1 && dice <= 6))
  assert(dices.nonEmpty && dices.size <=3)

  override val systemOnly: Boolean = true
}

case class MoveInToConqueredTerritory(noOfTroops: Int) extends RiskAction

case class ManeuverTroops(from: Int, to: Int, noOfTroops: Int) extends RiskAction {
  assert(from >= 0)
  assert(to >= 0)
  assert(noOfTroops >= 1)
}

case object AbandonCurrentCampaign extends RiskAction

case object Skip extends RiskAction
