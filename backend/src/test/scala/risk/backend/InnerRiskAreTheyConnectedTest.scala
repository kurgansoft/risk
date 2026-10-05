package risk.backend

import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0.{Blue, ColorMap, Green, Red, White}

class InnerRiskAreTheyConnectedTest extends AnyFunSuite {

  val ir = InnerRisk.generate(ColorMap(Map(Green -> 1, Red -> 2, Blue -> 3, White -> 4)), 1L)

  test("all areas are connected to themselves") {
    for (i <- ir.territories.indices)
      assert(ir.areTheyConnected(i, i))
  }

  test("all areas are connected to the neighbours that are owned by the same roleid") {
    for (i <- ir.territories.indices) {
      val territory = ir.territories(i)
      val neighboursUnderTheSameRule = ir.ts(i).neighbours.map(x => (x, ir.territories(x))).filter(_._2.roleId == territory.roleId)
      for (neighbour <- neighboursUnderTheSameRule) {
        assert(ir.areTheyConnected(neighbour._1, i))
      }
    }
  }

  test("two territory can never be connected if they are under different rule") {
    for (i <- ir.territories.indices) {
      val territory = ir.territories(i)
      val roleId = territory.roleId
      val allTheTerritoriesUnderDifferentRule = ir.territories.zipWithIndex.filter(_._1.roleId != roleId).map(_._2)
      for (index <- allTheTerritoriesUnderDifferentRule) {
        assert(!ir.areTheyConnected(index, i))
      }
    }
  }

  test("The function 'areTheyConnected' is commutative") {
    for (i <- ir.territories.indices) {
      for (j <- ir.territories.indices) {
        ir.areTheyConnected(i, j) == ir.areTheyConnected(j, i)
      }
    }
  }

}
