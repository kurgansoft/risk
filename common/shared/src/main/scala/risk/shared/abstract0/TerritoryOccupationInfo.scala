package risk.shared.abstract0

import zio.blocks.schema.Schema

case class TerritoryOccupationInfo(
    roleId: Int,
    noOfUnits: Int = 0,
    noOfProposedUnits: Int = 0,
    noOfUnitsInBattle: Int = 0
) {
  def resolveBattle(
      lostTroops: Int,
      remainingTroopsInBattle: Int
  ): TerritoryOccupationInfo = {
    copy(
      noOfUnitsInBattle = remainingTroopsInBattle,
      noOfUnits = noOfUnits - lostTroops
    )
  }

  def normalize(): TerritoryOccupationInfo = {
    copy(noOfUnitsInBattle = 0)
  }
}

object TerritoryOccupationInfo {
  implicit val schema: Schema[TerritoryOccupationInfo] = Schema.derived
}
