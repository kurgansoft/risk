package risk.shared.abstract0

import risk.shared.abstract0.GamePhase.*

abstract class AbstractInnerRisk(
                                  val territories: List[TerritoryOccupationInfo] =
      List.empty, // x2(roleId, noOfUnits)
                                  val players: Map[RiskRole, AbstractRiskPlayerInfo],
                                  val roleOfTheCurrentPlayer: RiskRole,
                                  val phase: GamePhase = AddingUnits,
                                  val campaign: Option[AbstractCampaign] = None,
                                  val round: Int = 0,
                                  val noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer: Int = 0
) {

  lazy val arrow: Option[(Int, Int)] =
    campaign.map(c => (c.source, c.destination))

  lazy val defendersRole: RiskRole = {
    assert(phase == BattleAndManeuver)
    assert(campaign.isDefined)
    val targetTerritory = campaign.get.destination
    territories(targetTerritory).role
  }

  lazy val attackersRole: RiskRole = {
    assert(phase == BattleAndManeuver)
    assert(campaign.isDefined)
    val targetTerritory = campaign.get.source
    territories(targetTerritory).role
  }

  def getPlayerById(role: RiskRole): Option[AbstractRiskPlayerInfo] =
    players.get(role)

  lazy val getCurrentPlayer: AbstractRiskPlayerInfo =
    players(roleOfTheCurrentPlayer)

  private def calculateNoOfNewTroopsForRole(role: RiskRole): Int = {
    val noOfControlledTerritories: Int = territories.count(_.role == role)
    val divided: Int = noOfControlledTerritories / 2
    Math.max(3, divided)
  }

  lazy val calculateNoOfNewTroops: Int = calculateNoOfNewTroopsForRole(
    roleOfTheCurrentPlayer
  )

  def areTheyNeighbours(one: Int, other: Int): Boolean =
    ts(one).neighbours.contains(other)

  /** Two territory is considered connected if they are occupied by the same
    * role and a path exist between them that all the nodes on it are also
    * occupied by the same role.
    * @param t1
    * @param t2
    * @return
    */

  def areTheyConnected(t1: Int, t2: Int): Boolean = {
    if (t1 < 0 || t1 >= ts.size || t2 < 0 || t2 >= ts.size) {
      false
    } else if (t1 == t2) {
      true
    } else {
      val owner = territories(t1).role
      if (territories(t2).role != owner) {
        false
      } else {
        var alreadyReached = Set(t1)
        var exhaustedOnes: Set[Int] = Set.empty
        var before: Int = 0
        var after: Int = 0
        def doStuff(): Unit = {
          var continue = true
          while (continue) {
            val toExplore = alreadyReached -- exhaustedOnes
            before = alreadyReached.size
            for (t <- toExplore) {
              alreadyReached = alreadyReached ++ ts(t).neighbours.filter(
                territories(_).role == owner
              )
              exhaustedOnes = exhaustedOnes + t
            }
            after = alreadyReached.size
            continue = before != after
          }
        }
        doStuff()
        alreadyReached.contains(t2)
      }
    }
  }

  val ts: List[Territory] = List(
    Territory("Győr-Moson-Sopron megye", Set(1, 2, 3)), // 0
    Territory("Vas megye", Set(0, 2, 4)), // 1
    Territory("Veszprém megye", Set(0, 1, 3, 4, 5, 6)), // 2
    Territory("Komárom-Esztergom megye", Set(0, 2, 6, 7)), // 3
    Territory("Zala megye", Set(1, 2, 5)), // 4
    Territory("Somogy megye", Set(4, 2, 6, 8, 9)), // 5
    Territory("Fejér megye", Set(3, 2, 5, 8, 10, 7)), // 6
    Territory("Pest megye", Set(3, 6, 10, 11, 12, 13, 14)), // 7
    Territory("Tolna megye", Set(6, 10, 9, 5)), // 8
    Territory("Baranya megye", Set(5, 8, 10)), // 9
    Territory("Bács-Kiskun megye", Set(9, 8, 6, 7, 12, 15)), // 10
    Territory("Budapest", Set(7)), // 11
    Territory(
      "Jász-Nagykun-Szolnok megye",
      Set(15, 10, 7, 13, 17, 16, 18)
    ), // 12
    Territory("Heves megye", Set(14, 7, 12, 16)), // 13
    Territory("Nógrád megye", Set(7, 13)), // 14
    Territory("Csongrád megye", Set(10, 12, 18)), // 15
    Territory("Borsod-Abaúj-Zemplén megye", Set(13, 12, 16, 17, 19)), // 16
    Territory("Hajdú-Bihar megye", Set(19, 18, 12, 16)), // 17
    Territory("Békés megye", Set(15, 12, 17)), // 18
    Territory("Szabolcs-Szatmár-Bereg megye", Set(16, 17)) // 19
  )
}
