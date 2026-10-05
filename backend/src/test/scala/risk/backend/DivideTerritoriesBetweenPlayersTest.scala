package risk.backend

import gbge.shared.GameState
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0._

class DivideTerritoriesBetweenPlayersTest extends AnyFunSuite {

  test("t1") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val result = risk0
      .reduce0(PickColor(1, Green), Some(1)).toOption.get._1
      .reduce0(PickColor(2, Red), Some(2)).toOption.get._1
      .reduce0(PickColor(3, White), Some(3)).toOption.get._1
      .reduce0(Init, Some(1), isAdmin = true).toOption.get

    assert(result._1.state == GameState.IN_PROGRESS)
    val result2: Risk = result._1.reduce0(DivideTerritoriesBetweenPlayers(1L), None).toOption.get._1
    println(result2)
    println(result2)
  }
}
