package risk.backend

import risk.shared.abstract0.{ColorMap, Red, White, Green, Blue}
import zio.test.{ZIOSpecDefault, assertTrue}

object RiskMapTest extends ZIOSpecDefault {
  override def spec = suite("RiskMapTest")(
    test("territory neighbours are symmetric") {
      val ir = InnerRisk.generate(ColorMap(Map(Green -> 1, Red -> 2, Blue -> 3, White -> 4)), 1L)
      val territories = ir.ts

      assertTrue(
        territories.zipWithIndex.forall { case (territory, index) =>
          territory.neighbours.forall { neighbour =>
            territories(neighbour).neighbours.contains(index)
          }
        }
      )
    }
  )
}
