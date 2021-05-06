package risk.backend

import gbge.shared.IN_PROGRESS
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0._

class ProperPathTest extends AnyFunSuite {
  test("Init test - 1") {
    var action: RiskAction = Init
    var risk: Risk = Risk(3)
    assert(risk.colorMap.isEmpty)

    action = PickColor(1, Green)
    risk = risk.reduce0(action, Some(1))._1
    assert(risk.colorMap.size == 1)

    action = PickColor(2, Blue)
    risk = risk.reduce0(action, Some(2))._1
    assert(risk.colorMap.size == 2)

    action = PickColor(3, White)
    risk = risk.reduce0(action, Some(3))._1
    assert(risk.colorMap.size == 3)

    action = Init
    risk = risk.reduce0(action, Some(2), isAdmin = true)._1
    assert(risk.colorMap.size == 3)
    assert(risk.state == IN_PROGRESS)
    assert(risk.innerRisk.isEmpty)

    action = DivideTerritoriesBetweenPlayers(1L)
    risk = risk.reduce0(action, None)._1
    assert(risk.innerRisk.isDefined)

    val territoryId = 0
    action = AddUnitToTerritory(territoryId)
    assert(risk.innerRisk.isDefined)
    assert(risk.innerRisk.get.territories(territoryId).noOfUnits == 1)
    assert(risk.innerRisk.get.currentPlayer == 1)
    val x = risk.innerRisk.get.players.forall(_.troops == 10)
    assert(x)

    val t = risk.reduce0(action, Some(1))
    for (i <- 0 to 19) {
      println(s"Territory $i belongs to player with id " + risk.innerRisk.get.territories(i).roleId)
    }
    println(t._2)
    risk = t._1

    assert(risk.innerRisk.isDefined)
    assert(risk.innerRisk.get.territories(territoryId).noOfUnits == 2)
    assert(risk.innerRisk.get.players.exists(_.troops == 9))
    assert(risk.innerRisk.get.currentPlayer == 2)

    //------------------

    action = AddUnitToTerritory(2)
    risk = risk.reduce0(action, Some(2))._1

    action = AddUnitToTerritory(3)
    risk = risk.reduce0(action, Some(3))._1

    for (_ <- 1 to 9) {
      action = AddUnitToTerritory(1)
      risk = risk.reduce0(action, Some(1))._1

      action = AddUnitToTerritory(2)
      risk = risk.reduce0(action, Some(2))._1

      action = AddUnitToTerritory(3)
      risk = risk.reduce0(action, Some(3))._1
    }

  }
}
