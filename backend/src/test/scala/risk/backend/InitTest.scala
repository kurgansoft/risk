package risk.backend

import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0._

class InitTest extends AnyFunSuite {

  test("Init test - 1") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val risk1 = risk0.reduce0(Init, None).toOption.get._1
    assert(risk0 == risk1)
  }

  test("Init test; it works") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val result = risk0
                     .reduce0(PickColor(1, Green), Some(1)).toOption.get._1
                     .reduce0(PickColor(1, Red), Some(1)).toOption.get._1
                     .reduce0(PickColor(1, White), Some(1)).toOption.get._1
                     .reduce0(Init, Some(1))
    println(s"risk1: $result")
  }
}
