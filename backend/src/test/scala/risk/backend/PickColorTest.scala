package risk.backend

import gbge.shared.GameState
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0.{Green, PickColor, Red, White}

class PickColorTest extends AnyFunSuite {

  test("PickColor -> with no authentication") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    assert(risk0.reduce0(PickColor(1, Green), None).isLeft)
  }

  test("PickColor -> can't do if game is started already") {
    val risk0: Risk = Risk(3, state = GameState.IN_PROGRESS)
    assert(risk0.colorMap.isEmpty)
    assert(risk0.reduce0(PickColor(1, Green), Some(1)).isLeft)
  }

  test("PickColor -> can't do if authenticated but not 'owner'") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    assert(risk0.reduce0(PickColor(1, Green), Some(2)).isLeft)
  }

  test("PickColor -> can do if authenticated but not 'owner' but admin") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val risk1 = risk0.reduce0(PickColor(1, Green), Some(2), isAdmin = true).toOption.get._1
    assert(risk1.colorMap.size == 1)
  }

  test("PickColor -> cool") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val risk2 = risk0.reduce0(PickColor(1, Green), Some(1)).toOption.get._1
    assert(risk2.colorMap.size == 1)
  }

  test("PickColor -> cool; changing color") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val risk1 = risk0.reduce0(PickColor(1, Green), Some(1)).toOption.get._1
    assert(risk1.colorMap.size == 1)
    assert(risk1.colorMap.roleOfColor(Green).contains(1))

    val risk2 = risk1.reduce0(PickColor(1, Red), Some(1)).toOption.get._1
    assert(risk2.colorMap.size == 1)
    assert(risk2.colorMap.roleOfColor(Red).contains(1))
  }

  test("PickColor -> cool, then trouble") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val risk1 = risk0.reduce0(PickColor(1, Green), Some(1)).toOption.get._1
    assert(risk1.colorMap.size == 1)

    assert(risk1.reduce0(PickColor(1, Green), Some(1)).isLeft)
    assert(risk1.reduce0(PickColor(2, Green), Some(2)).isLeft)
  }

  test("PickColor -> cool2") {
    val risk0: Risk = Risk(3)
    assert(risk0.colorMap.isEmpty)
    val risk2 = risk0
      .reduce0(PickColor(1, Green), Some(1)).toOption.get._1
      .reduce0(PickColor(2, White), Some(2)).toOption.get._1
      .reduce0(PickColor(3, Red), Some(3)).toOption.get._1
    assert(risk2.colorMap.size == 3)
  }
}
