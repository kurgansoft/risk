package risk.shared

import risk.shared.abstract0.{AbstractBattle, BattlePhase, DeclareNoOfTroopsPhase}
import upickle.default.{macroRW, ReadWriter => RW}

case class ClientBattle(
                         override val noOfAttackers: Int,
                         override val noOfDefenders: Int,
                         override val phase: BattlePhase = DeclareNoOfTroopsPhase,
                         override val attackerDieRolling: Boolean,
                         override val defenderDieRolling: Boolean,
                         override val attackerDie: Option[List[Int]],
                         override val defenderDie: Option[List[Int]]
                           ) extends AbstractBattle(noOfAttackers, noOfDefenders, phase, attackerDieRolling, defenderDieRolling, attackerDie, defenderDie)

object ClientBattle {
  implicit def rw: RW[ClientBattle] = macroRW
}
