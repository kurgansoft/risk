package risk.backend

import gbge.backend.BackendGameProps
import gbge.backend.models.Universe
import gbge.backend.services.{SequentialTokenGenerator, TokenGenerator}
import gbge.shared.actions._
import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue}

object StartTest extends ZIOSpecDefault {
  override def spec = suite("StartTest")(
    test("starts a game after players join") {
      val games: Seq[BackendGameProps[?, ?]] = Seq(BackendRiskProps)

      def applyAction(universe: Universe, action: Action, playerId: Option[Int]): ZIO[TokenGenerator, Nothing, Universe] = {
        universe.reduce(action, playerId) match {
          case Left(_) => ZIO.succeed(universe)
          case Right((newUniverse, effect)) =>
            effect.flatMap {
              case Some(followUp) => applyAction(newUniverse, followUp, playerId)
              case None => ZIO.succeed(newUniverse)
            }
        }
      }

      val program = for {
        u1 <- applyAction(Universe(games), Join("Player1"), None)
        u2 <- applyAction(u1, Join("Player2"), None)
        u3 <- applyAction(u2, Join("Player3"), None)
        u4 <- applyAction(u3, SelectGame(0), Some(1))
        u5 <- applyAction(u4, Start, Some(1))
        _ <- zio.Console.printLine("u5 is: " + u5)
      } yield u5

      program.provideLayer(SequentialTokenGenerator.defaultLayer).as(assertTrue(true))
    }
  )
}
