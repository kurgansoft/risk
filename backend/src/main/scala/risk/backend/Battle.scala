package risk.backend

import gbge.backend.{Failure, GeneralFailure}
import risk.shared.ClientBattle
import risk.shared.abstract0._

import scala.language.implicitConversions

case class Battle(
    override val noOfAttackers: Int = 0,
    override val noOfDefenders: Int = 0,
    override val phase: BattlePhase = DeclareNoOfTroopsPhase,
    override val attackerDieRolling: Boolean = false,
    override val defenderDieRolling: Boolean = false,
    override val attackerDie: Option[List[Int]] = None,
    override val defenderDie: Option[List[Int]] = None
) extends AbstractBattle(
      noOfAttackers,
      noOfDefenders,
      phase,
      attackerDieRolling,
      defenderDieRolling,
      attackerDie,
      defenderDie
    ) {

  implicit def convert(battle: Battle): Either[Failure, Battle] = Right(battle)

  def toClientBattle(): ClientBattle = {
    ClientBattle(
      noOfAttackers,
      noOfDefenders,
      phase,
      attackerDieRolling,
      defenderDieRolling,
      attackerDie.map(_.sorted.reverse),
      defenderDie.map(_.sorted.reverse)
    )
  }

  def reduce(battleAction: CampaignAction): Either[Failure, Battle] = {
    battleAction match {
      case Attack(attackers) => {
        if (phase == DeclareNoOfTroopsPhase && this.noOfAttackers == 0)
          this.copy(noOfAttackers = attackers)
        else
          Left(GeneralFailure("___"))
      }
      case Defend(defenders) => {
        if (
          phase == DeclareNoOfTroopsPhase && this.noOfDefenders == 0 && this.noOfAttackers != 0
        )
          this.copy(noOfDefenders = defenders, phase = DiceThrowingPhase)
        else
          Left(GeneralFailure("___"))
      }
      case RollDiceForAttacker => {
        if (!attackerDieRolling && phase == DiceThrowingPhase)
          this.copy(attackerDieRolling = true)
        else
          Left(GeneralFailure(""))
      }
      case RollDiceForDefender => {
        if (!defenderDieRolling && phase == DiceThrowingPhase)
          this.copy(defenderDieRolling = true)
        else
          Left(GeneralFailure(""))
      }
      case DiceResultForAttacker(dices) => {
        if (dices.size != noOfAttackers) {
          Left(
            GeneralFailure(
              s"There are $noOfAttackers attackers, but ${dices.size} have been rolled."
            )
          )
        } else if (attackerDie.isDefined) {
          Left(GeneralFailure("Attacker dies have already been rolled."))
        } else {
          val temp =
            this.copy(attackerDie = Some(dices), attackerDieRolling = false)
          if (temp.defenderDie.isDefined && temp.attackerDie.isDefined) {
            temp.copy(phase = ResultPhase)
          } else {
            temp
          }
        }
      }
      case DiceResultForDefender(dices) => {
        if (dices.size != noOfDefenders) {
          Left(
            GeneralFailure(
              s"There are $noOfDefenders defenders, but ${dices.size} have been rolled."
            )
          )
        } else if (defenderDie.isDefined) {
          Left(GeneralFailure("Defender dies have already been rolled."))
        } else {
          val temp =
            this.copy(defenderDie = Some(dices), defenderDieRolling = false)
          if (temp.defenderDie.isDefined && temp.attackerDie.isDefined) {
            temp.copy(phase = ResultPhase)
          } else {
            temp
          }
        }
      }
      case _ => this
    }
  }
}
