package risk.shared

import risk.shared.abstract0.{AbstractCampaign, CampaignStatus}
import upickle.default.{macroRW, ReadWriter => RW}

case class ClientCampaign(
                   override val source: Int,
                   override val destination: Int,
                   override val battle: ClientBattle,
                   override val battleNumber: Int,
                   override val noOfTroopsLostByTheAttacker: Int,
                   override val noOfTroopsLostByTheDefender: Int,
                   override val campaignStatus: CampaignStatus
                 ) extends AbstractCampaign(source, destination, battle, battleNumber, noOfTroopsLostByTheAttacker, noOfTroopsLostByTheDefender, campaignStatus)

object ClientCampaign {
  implicit def rw: RW[ClientCampaign] = macroRW
}
