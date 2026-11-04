package risk.backend

import gbge.shared.GameState
import risk.shared.abstract0._
import zio.test.{ZIOSpecDefault, assertTrue}

object ProperPathTest extends ZIOSpecDefault {
  override def spec = suite("ProperPathTest")(
    test("plays through initialization and unit deployment") {
      val checks = List.newBuilder[Boolean]
      var action: RiskAction = Init
      var risk: Risk = Risk(3)
      checks += risk.colorMap.isEmpty

      action = PickColor(1, Green)
      risk = risk.reduce0(action, Some(1)).getOrElse(???)._1
      checks += (risk.colorMap.size == 1)

      action = PickColor(2, Blue)
      risk = risk.reduce0(action, Some(2)).getOrElse(???)._1
      checks += (risk.colorMap.size == 2)

      action = PickColor(3, White)
      risk = risk.reduce0(action, Some(3)).getOrElse(???)._1
      checks += (risk.colorMap.size == 3)

      action = Init
      risk = risk.reduce0(action, Some(2), isAdmin = true).getOrElse(???)._1
      checks += (risk.colorMap.size == 3)
      checks += (risk.state == GameState.IN_PROGRESS)
      checks += risk.innerRisk.isEmpty

      action = DivideTerritoriesBetweenPlayers(1L)
      risk = risk.reduce0(action, None).getOrElse(???)._1
      checks += risk.innerRisk.isDefined

      action = AddUnitToTerritory(1)
      checks += risk.innerRisk.isDefined
      checks += (risk.innerRisk.get.territories(1).noOfUnits == 1)
      checks += (risk.innerRisk.get.currentPlayerIndex == 1)
      checks += risk.innerRisk.get.players.forall(_.troops == 10)

      val result = risk.reduce0(action, Some(1)).toOption.get
      risk = result._1
      checks += risk.innerRisk.isDefined
      checks += (risk.innerRisk.get.territories(1).noOfUnits == 2)
      checks += risk.innerRisk.get.players.exists(_.troops == 9)
      checks += (risk.innerRisk.get.currentPlayerIndex == 2)

      action = AddUnitToTerritory(5)
      risk = risk.reduce0(action, Some(2)).toOption.get._1

      action = AddUnitToTerritory(3)
      risk = risk.reduce0(action, Some(3)).toOption.get._1

      for (_ <- 1 to 9) {
        action = AddUnitToTerritory(1)
        risk = risk.reduce0(action, Some(1)).toOption.get._1

        action = AddUnitToTerritory(5)
        risk = risk.reduce0(action, Some(2)).toOption.get._1

        action = AddUnitToTerritory(3)
        risk = risk.reduce0(action, Some(3)).toOption.get._1
      }
      checks += risk.innerRisk.isDefined

      assertTrue(checks.result().forall(identity))
    }
  )
}
