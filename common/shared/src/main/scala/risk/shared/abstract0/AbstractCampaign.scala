package risk.shared.abstract0

abstract sealed class CampaignStatus

case object RUNNING extends CampaignStatus
case object ABANDONED extends CampaignStatus
case object ATTACKER_LOST extends CampaignStatus
case object DEFENDER_LOST extends CampaignStatus

abstract class AbstractCampaign(
    val source: Int,
    val destination: Int,
    val battle: AbstractBattle,
    val battleNumber: Int = 1,
    val noOfTroopsLostByTheAttacker: Int = 0,
    val noOfTroopsLostByTheDefender: Int = 0,
    val campaignStatus: CampaignStatus = RUNNING
)
