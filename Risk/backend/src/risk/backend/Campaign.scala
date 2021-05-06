package risk.backend

import gbge.backend.{OK, UniverseResult}
import risk.shared.ClientCampaign
import risk.shared.abstract0.{AbstractCampaign, Attack, CampaignAction, CampaignStatus, RUNNING, ResultPhase}

case class Campaign(
                 override val source: Int,
                 override val destination: Int,
                 override val battle: Battle = Battle(),
                 override val battleNumber: Int = 1,
                 override val noOfTroopsLostByTheAttacker: Int = 0,
                 override val noOfTroopsLostByTheDefender: Int = 0,
                 override val campaignStatus: CampaignStatus = RUNNING
                 ) extends AbstractCampaign(source, destination, battle, battleNumber, noOfTroopsLostByTheAttacker, noOfTroopsLostByTheDefender, campaignStatus) {

  def toClientBattle(): ClientCampaign = ClientCampaign(source, destination, battle.toClientBattle(), battleNumber, noOfTroopsLostByTheAttacker, noOfTroopsLostByTheDefender, campaignStatus)

  def reduce(battleAction: CampaignAction): (Campaign, UniverseResult) = {
    val temp = if (battleAction.isInstanceOf[Attack] && battle.phase == ResultPhase) {
      this.copy(battle = Battle(), battleNumber = battleNumber+1)
    } else this
    val (newBattle, result) = temp.battle.reduce(battleAction)
    if (newBattle.phase == ResultPhase && result == OK)
      (temp.copy(
        battle = newBattle,
        noOfTroopsLostByTheDefender = noOfTroopsLostByTheDefender + newBattle.noOfTroopsLostByTheDefender,
        noOfTroopsLostByTheAttacker = noOfTroopsLostByTheAttacker + newBattle.noOfTroopsLostByTheAttacker),
      result)
    else
      (temp.copy(battle = newBattle), result)
  }
}
