package risk.backend

import gbge.backend.Failure
import risk.shared.abstract0.{Attack, Defend, DiceResultForAttacker, DiceResultForDefender, ResultPhase, RollDiceForAttacker, RollDiceForDefender}
import zio.test.{ZIOSpecDefault, assertTrue}

object BattleTest extends ZIOSpecDefault {
  override def spec = suite("BattleTest")(
    test("test no. 0") {
      val battle = Battle()
      battle.invariant()
      val result = battle.reduce(Defend(1))

      val reducedBattle = battle.reduce(Attack(3)).toOption.get
      assertTrue(result.isLeft, reducedBattle.noOfAttackers == 3)
    },
    test("first test") {
      val battle = Battle(3)
      battle.invariant()

      var reducedBattle: Battle = battle.reduce(Defend(2)).toOption.get
      reducedBattle.invariant()
      val firstDefenderCount = reducedBattle.noOfDefenders

      reducedBattle = battle.reduce(Defend(1)).toOption.get
      reducedBattle.invariant()
      assertTrue(firstDefenderCount == 2, reducedBattle.noOfDefenders == 1)
    },
    test("AttackerDice1") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(DiceResultForAttacker(List(3, 2, 1))).toOption.get
      reducedBattle.invariant()
      assertTrue(
        reducedBattle.noOfDefenders == 2,
        reducedBattle.attackerDie.contains(List(3, 2, 1))
      )
    },
    test("XXX") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(RollDiceForAttacker).toOption.get
      reducedBattle.invariant()
      assertTrue(reducedBattle.attackerDieRolling, !reducedBattle.defenderDieRolling)
    },
    test("XXX2") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(RollDiceForDefender).toOption.get
      reducedBattle.invariant()
      assertTrue(!reducedBattle.attackerDieRolling, reducedBattle.defenderDieRolling)
    },
    test("XXX3") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(RollDiceForDefender).toOption.get
      val reducedBattle2: Battle = reducedBattle.reduce(DiceResultForDefender(List(6, 3))).toOption.get
      reducedBattle2.invariant()
      assertTrue(
        !reducedBattle2.attackerDieRolling,
        !reducedBattle2.defenderDieRolling,
        reducedBattle2.defenderDie.isDefined
      )
    },
    test("AttackerDice2") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val result: Either[Failure, Battle] = battle.reduce(DiceResultForAttacker(List(2, 1)))
      assertTrue(result.isLeft)
    },
    test("AttackerDice3") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val result1 = battle.reduce(DiceResultForAttacker(List(3, 2, 1)))
      val reducedBattle = result1.getOrElse(???)
      reducedBattle.invariant()
      val result2 = reducedBattle.reduce(DiceResultForAttacker(List(1, 2, 3)))
      assertTrue(result1.isRight, result2.isLeft)
    },
    test("DefenderDice1") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(DiceResultForDefender(List(2, 1))).toOption.get
      reducedBattle.invariant()
      assertTrue(
        reducedBattle.noOfDefenders == 2,
        reducedBattle.defenderDie.contains(List(2, 1))
      )
    },
    test("DefenderDice2") {
      val battle = Battle(3).reduce(Defend(2)).getOrElse(???)
      battle.invariant()
      val result = battle.reduce(DiceResultForDefender(List(1)))
      assertTrue(result.isLeft)
    },
    test("DefenderDice3") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(DiceResultForDefender(List(2, 1))).getOrElse(???)
      reducedBattle.invariant()
      val result = reducedBattle.reduce(DiceResultForDefender(List(1, 2)))
      assertTrue(result.isLeft)
    },
    test("final test") {
      val battle = Battle(3).reduce(Defend(2)).toOption.get
      battle.invariant()
      val reducedBattle: Battle = battle.reduce(DiceResultForAttacker(List(3, 2, 1))).toOption.get
      reducedBattle.invariant()
      val reducedBattle2 = reducedBattle.reduce(DiceResultForDefender(List(1, 2))).toOption.get
      reducedBattle2.invariant()
      assertTrue(reducedBattle2.phase == ResultPhase)
    }
  )
}
