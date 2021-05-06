package risk.backend

import gbge.backend.{ExecuteAsyncEffect, GeneralFailure, OK, UniverseResult}
import risk.shared.ClientBattle
import risk.shared.abstract0._

case class Battle(
                   override val noOfAttackers: Int = 0,
                   override val noOfDefenders: Int = 0,
                   override val phase: BattlePhase = DeclareNoOfTroopsPhase,
                   override val attackerDieRolling: Boolean = false,
                   override val defenderDieRolling: Boolean = false,
                   override val attackerDie: Option[List[Int]] = None,
                   override val defenderDie: Option[List[Int]] = None)
  extends AbstractBattle(noOfAttackers, noOfDefenders, phase, attackerDieRolling, defenderDieRolling, attackerDie, defenderDie) {

  implicit def i1(battleCore: Battle): (Battle, UniverseResult) = (battleCore, OK)

  def toClientBattle(): ClientBattle = {
    ClientBattle(noOfAttackers, noOfDefenders, phase, attackerDieRolling, defenderDieRolling,
      attackerDie.map(_.sorted.reverse), defenderDie.map(_.sorted.reverse))
  }

  def reduce(battleAction: CampaignAction): (Battle, UniverseResult) = {
    battleAction match {
      case Attack(attackers) => {
        if (phase == DeclareNoOfTroopsPhase && this.noOfAttackers == 0)
          this.copy(noOfAttackers = attackers)
        else
          (this, GeneralFailure("___"))
      }
      case Defend(defenders) => {
        if (phase == DeclareNoOfTroopsPhase && this.noOfDefenders == 0 && this.noOfAttackers != 0)
          this.copy(noOfDefenders = defenders, phase = DiceThrowingPhase)
        else
          (this, GeneralFailure("___"))
      }
      case RollDiceForAttacker => {
        if (!attackerDieRolling && phase == DiceThrowingPhase)
          (this.copy(attackerDieRolling = true), ExecuteAsyncEffect(RiskEffects.attackerDiceEffect))
        else
          (this, GeneralFailure(""))
      }
      case RollDiceForDefender => {
        if (!defenderDieRolling && phase == DiceThrowingPhase)
          (this.copy(defenderDieRolling = true), ExecuteAsyncEffect(RiskEffects.defenderDiceEffect))
        else
          (this, GeneralFailure(""))
      }
      case DiceResultForAttacker(dices) => {
        if (dices.size != noOfAttackers) {
          (this, GeneralFailure(s"There are $noOfAttackers attackers, but ${dices.size} have been rolled."))
        } else if (attackerDie.isDefined) {
          (this, GeneralFailure("Attacker dies have already been rolled."))
        } else {
          val temp = this.copy(attackerDie = Some(dices), attackerDieRolling = false)
          if (temp.defenderDie.isDefined && temp.attackerDie.isDefined) {
            temp.copy(phase = ResultPhase)
          } else {
            temp.asInstanceOf[Battle]
          }
        }
      }
      case DiceResultForDefender(dices) => {
        if (dices.size != noOfDefenders) {
          (this, GeneralFailure(s"There are $noOfDefenders defenders, but ${dices.size} have been rolled."))
        } else if (defenderDie.isDefined) {
          (this, GeneralFailure("Defender dies have already been rolled."))
        } else {
          val temp = this.copy(defenderDie = Some(dices), defenderDieRolling = false)
          if (temp.defenderDie.isDefined && temp.attackerDie.isDefined) {
            temp.copy(phase = ResultPhase)
          } else {
            temp.asInstanceOf[Battle]
          }
        }
      }
      case _ => this
    }
  }
}
