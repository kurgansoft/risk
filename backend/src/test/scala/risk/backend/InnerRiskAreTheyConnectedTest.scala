package risk.backend

import risk.shared.abstract0.{Blue, ColorMap, Green, Red, White}
import zio.test.{ZIOSpecDefault, assertTrue}

object InnerRiskAreTheyConnectedTest extends ZIOSpecDefault {
  override def spec = {
    val ir = InnerRisk.generate(ColorMap(Map(Green -> 1, Red -> 2, Blue -> 3, White -> 4)), 1L)

    suite("InnerRiskAreTheyConnectedTest")(
      test("all areas are connected to themselves") {
        assertTrue(ir.territories.indices.forall(i => ir.areTheyConnected(i, i)))
      },
      test("all same-owner neighbours are connected") {
        assertTrue(ir.territories.indices.forall { i =>
            val territory = ir.territories(i)
            val neighboursUnderTheSameRule = ir.ts(i).neighbours
              .map(x => (x, ir.territories(x)))
              .filter(_._2.roleId == territory.roleId)
            neighboursUnderTheSameRule.forall { neighbour =>
              ir.areTheyConnected(neighbour._1, i)
            }
          })
      },
      test("territories owned by different players are not connected") {
        assertTrue(ir.territories.indices.forall { i =>
            val territory = ir.territories(i)
            val roleId = territory.roleId
            val differentOwners = ir.territories.zipWithIndex
              .filter(_._1.roleId != roleId)
              .map(_._2)
            differentOwners.forall(index => !ir.areTheyConnected(index, i))
          })
      },
      test("connectivity is commutative") {
        assertTrue(
          ir.territories.indices.forall { i =>
            ir.territories.indices.forall { j =>
              ir.areTheyConnected(i, j) == ir.areTheyConnected(j, i)
            }
          }
        )
      }
    )
  }
}
