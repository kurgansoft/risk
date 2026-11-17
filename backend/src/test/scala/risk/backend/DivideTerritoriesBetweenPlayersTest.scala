package risk.backend

import gbge.shared.GameState
import risk.shared.abstract0._
import risk.shared.abstract0.RiskColor.*
import zio.test.{ZIOSpecDefault, assertTrue}

object DivideTerritoriesBetweenPlayersTest extends ZIOSpecDefault {
  override def spec = suite("DivideTerritoriesBetweenPlayersTest")(
    test("divides territories after initialization") {
      val risk0: Risk = Risk(3)
      val result = risk0
        .reduce0(PickColor(1, Green), Some(1)).toOption.get._1
        .reduce0(PickColor(2, Red), Some(2)).toOption.get._1
        .reduce0(PickColor(3, White), Some(3)).toOption.get._1
        .reduce0(Init, Some(1), isAdmin = true).toOption.get

      val result2: Risk = result._1.reduce0(DivideTerritoriesBetweenPlayers(1L), None).toOption.get._1
      assertTrue(risk0.colorMap.isEmpty, result._1.state == GameState.IN_PROGRESS, result2.innerRisk.isDefined)
    }
  )
}
