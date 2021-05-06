package risk.backend

import gbge.backend.FailureWithMessage
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0.{ATTACKER_LOST, Attack, BattleAndManeuver, ColorMap, Defend, DeployingUnits, DiceResultForAttacker, DiceResultForDefender, Green, LaunchCampaign, ManeuverTroops, Red, RollDiceForAttacker, RollDiceForDefender, White}

class ManeuverTroopsTest extends AnyFunSuite {

  val ir = InnerRisk.generate(ColorMap(Map(Green -> 1, Red -> 2, White -> 3)), 2156796245064986980L)
  val ir2 = ir.copy(phase = BattleAndManeuver, territories = ir.territories.zipWithIndex.map(t => {
    if (t._2 == 5)
      t._1.copy(roleId = 1)
    else if (t._2 == 1)
      t._1.copy(noOfUnits = 8)
    else if (t._2 == 2)
      t._1.copy(noOfUnits = 3)
    else
      t._1
  }))

  test("general") {
    assert(ir2.territories(1).roleId == 1 && ir2.territories(1).noOfUnits == 8)
    assert(ir2.territories(5).roleId == 1 && ir2.territories(5).noOfUnits == 1)
    assert(ir2.territories(8).roleId == 1 && ir2.territories(8).noOfUnits == 1)
    assert(ir2.currentPlayer == 1)
    assert(ir2.phase == BattleAndManeuver)
  }

  test("good ones") {
//    val ir3 = ir2.reduce(ManeuverTroops(1,8,7), Some(1))._1
    for (source <- ir2.territories.zipWithIndex.filter(x => x._1.roleId ==1 && x._1.noOfUnits > 1)) {
      val noOfTroopsToManeuver = source._1.noOfUnits - 1
      for (destination <- ir2.territories.zipWithIndex.filter(_._1.roleId ==1).filter(x => ir2.areTheyConnected(x._2, source._2)).filterNot(_._2 == source._2)) {
        val iii = ir2.reduce(ManeuverTroops(source._2, destination._2, noOfTroopsToManeuver), Some(1))._1
        assert(iii.territories(source._2).roleId == 1 && iii.territories(source._2).noOfUnits == 1)
        assert(iii.territories(destination._2).roleId == 1 && iii.territories(destination._2).noOfUnits == noOfTroopsToManeuver + ir2.territories(destination._2).noOfUnits)
        assert(iii.currentPlayer == 2)
        assert(iii.phase == DeployingUnits)
      }
    }
  }

  test("good ones2") {
    val campaignThatWasLost = ir2.
      reduce(LaunchCampaign(1,0), Some(1))._1.
      reduce(Attack(3), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3,3,3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1.

      reduce(Attack(3), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3,3,3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1.

      reduce(Attack(3), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3,3,3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1.

      reduce(Attack(1), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1.

      reduce(Attack(1), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1.

      reduce(Attack(1), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1.

      reduce(Attack(1), Some(1))._1.
      reduce(Defend(1), Some(2))._1.
      reduce(RollDiceForAttacker, Some(1))._1.
      reduce(DiceResultForAttacker(List(3)), None)._1.
      reduce(RollDiceForDefender, Some(2))._1.
      reduce(DiceResultForDefender(List(3)), Some(2))._1

    assert(campaignThatWasLost.campaign.map(_.campaignStatus).contains(ATTACKER_LOST))

//    val ir7 = campaignThatWasLost.reduce(ManeuverTroops(1,8, 7), Some(1))._1
    for (source <- campaignThatWasLost.territories.zipWithIndex.filter(x => x._1.roleId ==1 && x._1.noOfUnits > 1)) {
      val noOfTroopsToManeuver = source._1.noOfUnits - 1
      for (destination <- campaignThatWasLost.territories.zipWithIndex.filter(_._1.roleId ==1).filter(x => ir2.areTheyConnected(x._2, source._2)).filterNot(_._2 == source._2)) {
        val iii = campaignThatWasLost.reduce(ManeuverTroops(source._2, destination._2, noOfTroopsToManeuver), Some(1))._1
        assert(iii.territories(source._2).roleId == 1 && iii.territories(source._2).noOfUnits == 1)
        assert(iii.territories(destination._2).roleId == 1 && iii.territories(destination._2).noOfUnits == noOfTroopsToManeuver + campaignThatWasLost.territories(destination._2).noOfUnits)
        assert(iii.currentPlayer == 2)
        assert(iii.phase == DeployingUnits)
      }
    }
  }

  test("wrong phase error") {
    val irx = ir2.copy(phase = DeployingUnits)
    val (newInnerRisk, error) = irx.reduce(ManeuverTroops(1,8,7), Some(1))
    assert(newInnerRisk == irx)
    assert(error.isInstanceOf[FailureWithMessage])
    println(error.asInstanceOf[FailureWithMessage].message)
    assert(error.asInstanceOf[FailureWithMessage].message == "Wrong phase")
  }

  test("authentication error") {
    val (newInnerRisk, error) = ir2.reduce(ManeuverTroops(1,8,7), None)
    val (newInnerRisk2, error2) = ir2.reduce(ManeuverTroops(1,8,7), Some(2))
    val (newInnerRisk3, error3) = ir2.reduce(ManeuverTroops(1,8,7), Some(3))
    val (newInnerRisk4, error4) = ir2.reduce(ManeuverTroops(1,8,7), Some(4))
    assert(newInnerRisk == ir2)
    assert(newInnerRisk2 == ir2)
    assert(newInnerRisk3 == ir2)
    assert(newInnerRisk4 == ir2)
    assert(error.isInstanceOf[FailureWithMessage])
    assert(error.asInstanceOf[FailureWithMessage].message == "Authentication error.")

    assert(error2.isInstanceOf[FailureWithMessage])
    assert(error2.asInstanceOf[FailureWithMessage].message == "Authentication error.")

    assert(error3.isInstanceOf[FailureWithMessage])
    assert(error3.asInstanceOf[FailureWithMessage].message == "Authentication error.")

    assert(error4.isInstanceOf[FailureWithMessage])
    assert(error4.asInstanceOf[FailureWithMessage].message == "Authentication error.")
  }

  test("unresolved battle") {
    val irx = ir2.reduce(LaunchCampaign(1,0), Some(1))._1
    val (newInnerRisk, error) = irx.reduce(ManeuverTroops(1,2,1), Some(1))
    assert(newInnerRisk == irx)
    assert(error.isInstanceOf[FailureWithMessage])
    assert(error.asInstanceOf[FailureWithMessage].message == "The current campaign needs to be resolved first.")
  }

  test("source is not under your command") {
    val (ir, error) = ir2.reduce(ManeuverTroops(6,8,1), Some(1))
    assert(ir == ir2)
    println(error.asInstanceOf[FailureWithMessage].message)
  }

  test("destination is not under your command") {
    val (ir, error) = ir2.reduce(ManeuverTroops(1,0,1), Some(1))
    assert(ir == ir2)
    println(error.asInstanceOf[FailureWithMessage].message)
  }

  test("Too many troops") {
    val (ir, error) = ir2.reduce(ManeuverTroops(1,8,8), Some(1))
    assert(ir == ir2)
    println(error.asInstanceOf[FailureWithMessage].message)
  }

  test("Territories are not connected.") {
    val (ir, error) = ir2.reduce(ManeuverTroops(1,18,1), Some(1))
    assert(ir == ir2)
    println(error.asInstanceOf[FailureWithMessage].message)
  }

}
