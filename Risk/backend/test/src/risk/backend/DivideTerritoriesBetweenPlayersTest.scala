package risk.backend

import gbge.shared.IN_PROGRESS
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0._

class DivideTerritoriesBetweenPlayersTest extends AnyFunSuite {

  test("t1") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val result = risk0
      .reduce0(PickColor(1, Green), Some(1))._1
      .reduce0(PickColor(2, Red), Some(2))._1
      .reduce0(PickColor(3, White), Some(3))._1
      .reduce0(Init, Some(1), isAdmin = true)

    assert(result._1.state == IN_PROGRESS)
    val result2: Risk = result._1.reduce0(DivideTerritoriesBetweenPlayers(1L), None)._1
    println(result2)
    println(result2)
  }
}
