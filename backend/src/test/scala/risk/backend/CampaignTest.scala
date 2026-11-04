package risk.backend

import risk.shared.abstract0.Attack
import zio.test.{ZIOSpecDefault, assertTrue}

object CampaignTest extends ZIOSpecDefault {
  override def spec = suite("CampaignTest")(
    test("attack starts a battle with the requested number of attackers") {
      val campaign = Campaign(1, 2)
      val result = campaign.reduce(Attack(2))
      assertTrue(result.toOption.get.battle.noOfAttackers == 2)
    }
  )
}
