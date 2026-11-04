package risk.backend

import gbge.backend.Failure
import risk.shared.ClientCampaign
import risk.shared.abstract0.{
  AbstractCampaign,
  Attack,
  CampaignAction,
  CampaignStatus,
  RUNNING,
  ResultPhase
}

import scala.language.implicitConversions

case class Campaign(
    override val source: Int,
    override val destination: Int,
    override val battle: Battle = Battle(),
    override val battleNumber: Int = 1,
    override val noOfTroopsLostByTheAttacker: Int = 0,
    override val noOfTroopsLostByTheDefender: Int = 0,
    override val campaignStatus: CampaignStatus = RUNNING
) extends AbstractCampaign(
      source,
      destination,
      battle,
      battleNumber,
      noOfTroopsLostByTheAttacker,
      noOfTroopsLostByTheDefender,
      campaignStatus
    ) {

  implicit def convert(campaign: Campaign): Either[Failure, Campaign] = Right(
    campaign
  )

  def toClientBattle(): ClientCampaign = ClientCampaign(
    source,
    destination,
    battle.toClientBattle(),
    battleNumber,
    noOfTroopsLostByTheAttacker,
    noOfTroopsLostByTheDefender,
    campaignStatus
  )

  def reduce(battleAction: CampaignAction): Either[Failure, Campaign] = {
    val temp =
      if (battleAction.isInstanceOf[Attack] && battle.phase == ResultPhase) {
        this.copy(battle = Battle(), battleNumber = battleNumber + 1)
      } else this
    temp.battle.reduce(battleAction) match {
      case Left(failure)    => Left(failure)
      case Right(newBattle) =>
        if (newBattle.phase == ResultPhase)
          temp.copy(
            battle = newBattle,
            noOfTroopsLostByTheDefender =
              noOfTroopsLostByTheDefender + newBattle.noOfTroopsLostByTheDefender,
            noOfTroopsLostByTheAttacker =
              noOfTroopsLostByTheAttacker + newBattle.noOfTroopsLostByTheAttacker
          )
        else
          temp.copy(battle = newBattle)
    }
  }
}
