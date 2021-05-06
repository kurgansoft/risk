package risk.backend

import gbge.backend.Failure
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0.{Attack, Defend, DiceResultForAttacker, DiceResultForDefender, ResultPhase, RollDiceForAttacker, RollDiceForDefender}

class BattleTest extends AnyFunSuite {

  test("test no. 0") {
    val battle = Battle()
    battle.invariant()
    val result = battle.reduce(Defend(1))
    assert(result._2.isInstanceOf[Failure])

    val reducedBattle = battle.reduce(Attack(3))._1
    assert(reducedBattle.noOfAttackers == 3)
  }

  test("first test") {
    val battle = Battle(3)
    battle.invariant()

    var reducedBattle: Battle = battle.reduce(Defend(2))._1
    reducedBattle.invariant()
    assert(reducedBattle.noOfDefenders == 2)

    reducedBattle = battle.reduce(Defend(1))._1
    reducedBattle.invariant()
    assert(reducedBattle.noOfDefenders == 1)
  }

  test("AttackerDice1") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForAttacker(List(3,2,1)))._1
    reducedBattle.invariant()
    assert(reducedBattle.noOfDefenders == 2)
    assert(reducedBattle.attackerDie.contains(List(3,2,1)))
  }

  //TODO Rename, refactor and extends these tests.
  test("XXX") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(RollDiceForAttacker)._1
    reducedBattle.invariant()
    assert(reducedBattle.attackerDieRolling)
    assert(!reducedBattle.defenderDieRolling)
  }

  test("XXX2") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(RollDiceForDefender)._1
    reducedBattle.invariant()
    assert(!reducedBattle.attackerDieRolling)
    assert(reducedBattle.defenderDieRolling)
  }

  test("XXX3") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(RollDiceForDefender)._1
    val reducedBattle2: Battle = reducedBattle.reduce(DiceResultForDefender(List(6,3)))._1
    reducedBattle2.invariant()
    assert(!reducedBattle2.attackerDieRolling)
    assert(!reducedBattle2.defenderDieRolling)
    assert(reducedBattle2.defenderDie.isDefined)
  }

  test("AttackerDice2") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForAttacker(List(2,1)))._1
    reducedBattle.invariant()
    assert(reducedBattle == battle)
  }

  test("AttackerDice3") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForAttacker(List(3,2,1)))._1
    reducedBattle.invariant()

    val reducedBattle2 = reducedBattle.reduce(DiceResultForAttacker(List(1,2,3)))._1
    reducedBattle2.invariant()

    assert(reducedBattle2 == reducedBattle)
  }

  test("DefenderDice1") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForDefender(List(2,1)))._1
    reducedBattle.invariant()
    assert(reducedBattle.noOfDefenders == 2)
    assert(reducedBattle.defenderDie.contains(List(2,1)))
  }

  test("DefenderDice2") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForDefender(List(1)))._1
    reducedBattle.invariant()
    assert(reducedBattle == battle)
  }

  test("DefenderDice3") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForDefender(List(2,1)))._1
    reducedBattle.invariant()

    val reducedBattle2 = reducedBattle.reduce(DiceResultForDefender(List(1,2)))._1
    reducedBattle2.invariant()

    assert(reducedBattle2 == reducedBattle)
  }

  test("final test") {
    val battle = Battle(3).reduce(Defend(2))._1
    battle.invariant()

    val reducedBattle: Battle = battle.reduce(DiceResultForAttacker(List(3,2,1)))._1
    reducedBattle.invariant()

    val reducedBattle2 = reducedBattle.reduce(DiceResultForDefender(List(1,2)))._1
    reducedBattle2.invariant()

    assert(reducedBattle2.phase == ResultPhase)
  }
}
