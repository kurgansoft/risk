package risk.backend

import gbge.shared.GameState
import risk.shared.abstract0.{Green, PickColor, Red, White}
import zio.test.{ZIOSpecDefault, assertTrue}

object PickColorTest extends ZIOSpecDefault {
  override def spec = suite("PickColorTest")(
    test("rejects a pick without authentication") {
      val risk0: Risk = Risk(3)
      assertTrue(
        risk0.colorMap.isEmpty,
        risk0.reduce0(PickColor(1, Green), None).isLeft
      )
    },
    test("rejects a pick after the game has started") {
      val risk0: Risk = Risk(3, state = GameState.IN_PROGRESS)
      assertTrue(
        risk0.colorMap.isEmpty,
        risk0.reduce0(PickColor(1, Green), Some(1)).isLeft
      )
    },
    test("rejects a non-owner") {
      val risk0: Risk = Risk(3)
      assertTrue(
        risk0.colorMap.isEmpty,
        risk0.reduce0(PickColor(1, Green), Some(2)).isLeft
      )
    },
    test("allows an admin to select another player's color") {
      val risk0: Risk = Risk(3)
      val risk1 = risk0.reduce0(PickColor(1, Green), Some(2), isAdmin = true).toOption.get._1
      assertTrue(risk0.colorMap.isEmpty, risk1.colorMap.size == 1)
    },
    test("allows a player to pick a color") {
      val risk0: Risk = Risk(3)
      val risk2 = risk0.reduce0(PickColor(1, Green), Some(1)).toOption.get._1
      assertTrue(risk0.colorMap.isEmpty, risk2.colorMap.size == 1)
    },
    test("allows a player to change their color") {
      val risk0: Risk = Risk(3)
      val risk1 = risk0.reduce0(PickColor(1, Green), Some(1)).toOption.get._1

      val risk2 = risk1.reduce0(PickColor(1, Red), Some(1)).toOption.get._1
      assertTrue(
        risk1.colorMap.size == 1,
        risk1.colorMap.roleOfColor(Green).contains(1),
        risk2.colorMap.size == 1,
        risk2.colorMap.roleOfColor(Red).contains(1)
      )
    },
    test("rejects duplicate color selections") {
      val risk0: Risk = Risk(3)
      val risk1 = risk0.reduce0(PickColor(1, Green), Some(1)).toOption.get._1

      assertTrue(
        risk1.colorMap.size == 1,
        risk1.reduce0(PickColor(1, Green), Some(1)).isLeft,
        risk1.reduce0(PickColor(2, Green), Some(2)).isLeft
      )
    },
    test("allows each player to select a color") {
      val risk0: Risk = Risk(3)
      val risk2 = risk0
        .reduce0(PickColor(1, Green), Some(1)).toOption.get._1
        .reduce0(PickColor(2, White), Some(2)).toOption.get._1
        .reduce0(PickColor(3, Red), Some(3)).toOption.get._1
      assertTrue(risk2.colorMap.size == 3)
    }
  )
}
