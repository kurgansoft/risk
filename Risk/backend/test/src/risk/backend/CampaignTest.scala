package risk.backend

import org.scalatest.funsuite.AnyFunSuite
import risk.shared.abstract0.Attack

class CampaignTest extends AnyFunSuite {

  test("test1") {
    val campaign = Campaign(1,2)
    val r = campaign.reduce(Attack(2))
    assert(r._1.battle.noOfAttackers == 2)
  }
}
