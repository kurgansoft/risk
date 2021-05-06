package risk.shared.abstract0

import gbge.shared.GameRole
import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class GamePhase
case object AddingUnits extends GamePhase
case object DeployingUnits extends GamePhase
case object BattleAndManeuver extends GamePhase
case object GameOver extends GamePhase

abstract sealed class RiskColor

object RiskColor {
  implicit def rw: RW[RiskColor] = macroRW
  val enumerateColors: List[RiskColor] = List(Red, White, Green, Blue, Yellow)
}

case object Red extends RiskColor {
  override def toString: String = "red"
}
case object White extends RiskColor {
  override def toString: String = "white"
}
case object Green extends RiskColor {
  override def toString: String = "green"
}
case object Blue extends RiskColor {
  override def toString: String = "blue"
}
case object Yellow extends RiskColor {
  override def toString: String = "yellow"
}

case class RiskRole(playerNumber: Int) extends GameRole {
  override val roleId: Int = playerNumber
}

object GamePhase {
  implicit def rw: RW[GamePhase] = macroRW
}

abstract class AbstractInnerRisk(
                                  val territories: List[TerritoryOccupationInfo] = List.empty, //x2(roleId, noOfUnits)
                                  val players: List[AbstractRiskPlayer] = List.empty,
                                  val currentPlayer: Int = 0, //the roleId of the current player
                                  val phase: GamePhase = AddingUnits,
                                  val campaign: Option[AbstractCampaign] = None,
                                  val round: Int = 0,
                                  val noOfTerritoriesConqueredInThisRoundByTheCurrentPlayer: Int = 0
                                ) {

  lazy val arrow: Option[(Int, Int)] = campaign.map(c => (c.source, c.destination))

  lazy val defendersRole: Int = {
    assert(phase == BattleAndManeuver)
    assert(campaign.isDefined)
    val targetTerritory = campaign.get.destination
    territories(targetTerritory).roleId
  }

  lazy val attackersRole: Int = {
    assert(phase == BattleAndManeuver)
    assert(campaign.isDefined)
    val targetTerritory = campaign.get.source
    territories(targetTerritory).roleId
  }

  def getPlayerById(playerId: Int): Option[AbstractRiskPlayer] = players.find(_.role == playerId)

  lazy val getCurrentPlayer: AbstractRiskPlayer = getPlayerById(currentPlayer).get

  def calculateNoOfNewTroopsForRole(roleId: Int): Int = {
    val noOfControlledTerritories: Int = territories.count(_.roleId == roleId)
    val divided: Int = noOfControlledTerritories/2
    Math.max(3, divided)
  }

  lazy val calculateNoOfNewTroops: Int = calculateNoOfNewTroopsForRole(currentPlayer)

  def areTheyNeighbours(one: Int, other: Int): Boolean = ts(one).neighbours.contains(other)

  /**
   * Two territory is considered connected if they are occupied by the same role and a path
   * exist between them that all the nodes on it are also occupied by the same role.
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
      val owner = territories(t1).roleId
      if (territories(t2).roleId != owner) {
        false
      } else {
        var alreadyReached = Set(t1)
        var exhaustedOnes: Set[Int] = Set.empty
        var before: Int = 0
        var after: Int = 0
        def doStuff(): Unit = {
          do {
            val toExplore = alreadyReached -- exhaustedOnes
            before = alreadyReached.size
            for (t <- toExplore) {
              alreadyReached = alreadyReached ++ ts(t).neighbours.filter(territories(_).roleId == owner)
              exhaustedOnes = exhaustedOnes + t
            }
            after = alreadyReached.size
          } while (before != after)
        }
        doStuff()
        alreadyReached.contains(t2)
      }
    }
  }

  val ts: List[Territory] = List(
    Territory("Győr-Moson-Sopron megye", Set(1,2,3)), //0
    Territory("Vas megye", Set(0,2,4)), // 1
    Territory("Veszprém megye", Set(0,1,3,4,5,6)), // 2
    Territory("Komárom-Esztergom megye", Set(0,2,6,7)), //3
    Territory("Zala megye", Set(1,2,5)), //4
    Territory("Somogy megye", Set(4,2,6,8,9)), //5
    Territory("Fejér megye", Set(3,2,5,8,10,7)), //6
    Territory("Pest megye", Set(3,6,10,11,12,13,14)), //7
    Territory("Tolna megye", Set(6,10,9,5)), //8
    Territory("Baranya megye", Set(5,8,10)), //9
    Territory("Bács-Kiskun megye", Set(9,8,6,7,12,15)), //10
    Territory("Budapest", Set(7)), //11
    Territory("Jász-Nagykun-Szolnok megye", Set(15,10,7,13,17,16,18)), // 12
    Territory("Heves megye", Set(14,7,12,16)), // 13
    Territory("Nógrád megye", Set(7,13)), //14
    Territory("Csongrád megye", Set(10,12,18)), //15
    Territory("Borsod-Abaúj-Zemplén megye", Set(13,12,16,17,19)), //16
    Territory("Hajdú-Bihar megye", Set(19,18,12,16)), // 17
    Territory("Békés megye", Set(15,12,17)), // 18
    Territory("Szabolcs-Szatmár-Bereg megye", Set(16,17)) //19
  )
}
