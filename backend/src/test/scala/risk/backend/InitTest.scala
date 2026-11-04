package risk.backend

import risk.shared.abstract0._
import zio.test.{ZIOSpecDefault, assertTrue}

object InitTest extends ZIOSpecDefault {
  override def spec = suite("InitTest")(
    test("init without authorization does not change the game") {
      val risk0: Risk = Risk(3)
      val result = risk0.reduce0(Init, None)
      assertTrue(risk0.colorMap.isEmpty, result.isLeft)
    },
    test("init after color selections") {
      val risk0: Risk = Risk(3)
      val result = risk0
        .reduce0(PickColor(1, Green), Some(1)).toOption.get._1
        .reduce0(PickColor(1, Red), Some(1)).toOption.get._1
        .reduce0(PickColor(1, White), Some(1)).toOption.get._1
        .reduce0(Init, Some(1))
      assertTrue(risk0.colorMap.isEmpty, result.isLeft)
    }
  )
}
