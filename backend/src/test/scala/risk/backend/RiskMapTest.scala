package risk.backend

import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0.{ColorMap, Red, White, Green, Blue}

class RiskMapTest  extends AnyFunSuite {
  test("test") {
    println("test1")
    val ir = InnerRisk.generate(ColorMap(Map(Green -> 1, Red -> 2, Blue -> 3, White -> 4)), 1L)
    val territories = ir.ts

    territories.zipWithIndex.foreach(t => {
      val territory = t._1
      val index = t._2

      territory.neighbours.foreach(n => {
        assert(territories(n).neighbours.contains(index))
      })
    })
  }
}
