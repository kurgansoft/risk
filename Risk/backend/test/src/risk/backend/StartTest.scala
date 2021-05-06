package risk.backend

import gbge.backend.Universe
import gbge.shared.actions._
import org.scalatest.funsuite.AnyFunSuite
import risk.shared.ClientRisk

class StartTest extends AnyFunSuite {
  test("test1") {

    gbge.shared.RG.registeredGames = List(ClientRisk)
    gbge.backend.RG.registeredGames = List(Risk)

    val token1: String = "token1"
    val token2: String = "token2"
    val token3: String = "token3"
    val actionList: List[(Action, Option[String])] = List(
      (Join("Player1"), None),
      (ProvideToken(token1),None),
      (Join("Player2"),None),
      (ProvideToken(token2),None),
      (Join("Player3"),None),
      (ProvideToken(token3),None),
      (SelectGame(0), Some(token1)),
      (Start, Some(token1)),
    )
    val u2 = actionList.foldLeft(Universe())((x,y) => x.reduce(y._1, y._2)._1)

    println("u2: " + u2)
  }
}
