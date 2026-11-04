package risk.backend

import gbge.backend.FailureWithMessage
import risk.shared.abstract0.*
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue}

object ManeuverTroopsTest extends ZIOSpecDefault {
  private val ir = InnerRisk.generate(ColorMap(Map(Green -> 1, Red -> 2, White -> 3)), 2156796245064986980L)
  private val ir2 = ir.copy(
    phase = BattleAndManeuver,
    territories = ir.territories.zipWithIndex.map { case (territory, index) =>
      if (index == 5)
        territory.copy(roleId = 1)
      else if (index == 1)
        territory.copy(noOfUnits = 7)
      else if (index == 2)
        territory.copy(noOfUnits = 3)
      else
        territory
    }
  )

  override def spec = suite("ManeuverTroopsTest")(
    test("general") {
      assertTrue(
        ir2.territories(1).roleId == 1 && ir2.territories(1).noOfUnits == 7,
        ir2.territories(5).roleId == 1 && ir2.territories(5).noOfUnits == 1,
        ir2.territories(8).roleId == 1 && ir2.territories(8).noOfUnits == 1,
        ir2.currentPlayerIndex == 1,
        ir2.phase == BattleAndManeuver
      )
    },

    test("allows valid troop maneuvers") {
      val maneuverResults = for {
        source <- ir2.territories.zipWithIndex.filter(x => x._1.roleId == 1 && x._1.noOfUnits > 1)
        destination <- ir2.territories.zipWithIndex
          .filter(_._1.roleId == 1)
          .filter(x => ir2.areTheyConnected(x._2, source._2))
          .filterNot(_._2 == source._2)
      } yield {
        val noOfTroopsToManeuver = source._1.noOfUnits - 1
        val result = ir2.reduce(ManeuverTroops(source._2, destination._2, noOfTroopsToManeuver), Some(1)).toOption.get
        result.territories(source._2).roleId == 1 &&
        result.territories(source._2).noOfUnits == 1 &&
        result.territories(destination._2).roleId == 1 &&
        result.territories(destination._2).noOfUnits == noOfTroopsToManeuver + ir2.territories(destination._2).noOfUnits &&
        result.currentPlayerIndex == 2 &&
        result.phase == DeployingUnits
      }

      assertTrue(maneuverResults.forall(identity))
    },
    test("allows maneuvers after a campaign is lost") {
      val campaignThatWasLost = ir2
        .reduce(LaunchCampaign(1, 0), Some(1)).toOption.get
        .reduce(Attack(3), Some(1)).toOption.get
        .reduce(Defend(1), Some(2)).toOption.get
        .reduce(RollDiceForAttacker, Some(1)).toOption.get
        .reduce(DiceResultForAttacker(List(3, 3, 3)), None).toOption.get
        .reduce(RollDiceForDefender, Some(2)).toOption.get
        .reduce(DiceResultForDefender(List(3)), Some(2)).toOption.get
        .reduce(Attack(3), Some(1)).toOption.get
        .reduce(Defend(1), Some(2)).toOption.get
        .reduce(RollDiceForAttacker, Some(1)).toOption.get
        .reduce(DiceResultForAttacker(List(3, 3, 3)), None).toOption.get
        .reduce(RollDiceForDefender, Some(2)).toOption.get
        .reduce(DiceResultForDefender(List(3)), Some(2)).toOption.get
        .reduce(Attack(3), Some(1)).toOption.get
        .reduce(Defend(1), Some(2)).toOption.get
        .reduce(RollDiceForAttacker, Some(1)).toOption.get
        .reduce(DiceResultForAttacker(List(3, 3, 3)), None).toOption.get
        .reduce(RollDiceForDefender, Some(2)).toOption.get
        .reduce(DiceResultForDefender(List(3)), Some(2)).toOption.get
        .reduce(Attack(1), Some(1)).toOption.get
        .reduce(Defend(1), Some(2)).toOption.get
        .reduce(RollDiceForAttacker, Some(1)).toOption.get
        .reduce(DiceResultForAttacker(List(3)), None).toOption.get
        .reduce(RollDiceForDefender, Some(2)).toOption.get
        .reduce(DiceResultForDefender(List(3)), Some(2)).toOption.get
        .reduce(Attack(1), Some(1)).toOption.get
        .reduce(Defend(1), Some(2)).toOption.get
        .reduce(RollDiceForAttacker, Some(1)).toOption.get
        .reduce(DiceResultForAttacker(List(3)), None).toOption.get
        .reduce(RollDiceForDefender, Some(2)).toOption.get
        .reduce(DiceResultForDefender(List(3)), Some(2)).toOption.get
        .reduce(Attack(1), Some(1)).toOption.get
        .reduce(Defend(1), Some(2)).toOption.get
        .reduce(RollDiceForAttacker, Some(1)).toOption.get
        .reduce(DiceResultForAttacker(List(3)), None).toOption.get
        .reduce(RollDiceForDefender, Some(2)).toOption.get
        .reduce(DiceResultForDefender(List(3)), Some(2)).toOption.get

      val maneuverResults = for {
        source <- campaignThatWasLost.territories.zipWithIndex
          .filter(x => x._1.roleId == 1 && x._1.noOfUnits > 1)
        destination <- campaignThatWasLost.territories.zipWithIndex
          .filter(_._1.roleId == 1)
          .filter(x => ir2.areTheyConnected(x._2, source._2))
          .filterNot(_._2 == source._2)
      } yield {
        val noOfTroopsToManeuver = source._1.noOfUnits - 1
        val result0 = campaignThatWasLost
          .reduce(ManeuverTroops(source._2, destination._2, noOfTroopsToManeuver), Some(1))
        val result = result0.getOrElse(???)
        result.territories(source._2).roleId == 1 &&
        result.territories(source._2).noOfUnits == 1 &&
        result.territories(destination._2).roleId == 1 &&
        result.territories(destination._2).noOfUnits == noOfTroopsToManeuver + campaignThatWasLost.territories(destination._2).noOfUnits &&
        result.currentPlayerIndex == 2 &&
        result.phase == DeployingUnits
      }

      assertTrue(
        campaignThatWasLost.campaign.map(_.campaignStatus).contains(ATTACKER_LOST),
        maneuverResults.forall(identity)
      )
    },
    test("rejects maneuvers in the wrong phase") {
      val irx = ir2.copy(phase = DeployingUnits)
      val Left(error) = irx.reduce(ManeuverTroops(1, 8, 7), Some(1)): @unchecked
      assertTrue(
        error.isInstanceOf[FailureWithMessage],
        error.asInstanceOf[FailureWithMessage].message == "Wrong phase"
      )
    },
    test("rejects unauthenticated and unauthorized players") {
      val Left(error) = ir2.reduce(ManeuverTroops(1, 8, 7), None): @unchecked
      val Left(error2) = ir2.reduce(ManeuverTroops(1, 8, 7), Some(2)): @unchecked
      val Left(error3) = ir2.reduce(ManeuverTroops(1, 8, 7), Some(3)): @unchecked
      val Left(error4) = ir2.reduce(ManeuverTroops(1, 8, 7), Some(4)): @unchecked
      assertTrue(
        error.isInstanceOf[FailureWithMessage] &&
          error.asInstanceOf[FailureWithMessage].message == "Authentication error.",
        error2.isInstanceOf[FailureWithMessage] &&
          error2.asInstanceOf[FailureWithMessage].message == "Authentication error.",
        error3.isInstanceOf[FailureWithMessage] &&
          error3.asInstanceOf[FailureWithMessage].message == "Authentication error.",
        error4.isInstanceOf[FailureWithMessage] &&
          error4.asInstanceOf[FailureWithMessage].message == "Authentication error."
      )
    },
    test("rejects a maneuver before the campaign is resolved") {
      val irx = ir2.reduce(LaunchCampaign(1, 0), Some(1)).toOption.get
      val Left(error) = irx.reduce(ManeuverTroops(1, 2, 1), Some(1)): @unchecked
      assertTrue(
        error.isInstanceOf[FailureWithMessage],
        error.asInstanceOf[FailureWithMessage].message == "The current campaign needs to be resolved first."
      )
    },
    test("rejects a source territory not under the player's command") {
      val Left(error) = ir2.reduce(ManeuverTroops(6, 8, 1), Some(1)): @unchecked
      assertTrue(error.isInstanceOf[FailureWithMessage])
    },
    test("rejects a destination territory not under the player's command") {
      val Left(error) = ir2.reduce(ManeuverTroops(1, 0, 1), Some(1)): @unchecked
      assertTrue(error.isInstanceOf[FailureWithMessage])
    },
    test("rejects moving too many troops") {
      val Left(error) = ir2.reduce(ManeuverTroops(1, 8, 8), Some(1)): @unchecked
      assertTrue(error.isInstanceOf[FailureWithMessage])
    },
    test("rejects maneuvers between unconnected territories") {
      val Left(error) = ir2.reduce(ManeuverTroops(1, 18, 1), Some(1)): @unchecked
      assertTrue(error.isInstanceOf[FailureWithMessage])
    }
  )
}
