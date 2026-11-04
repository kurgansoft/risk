package risk.backend

import gbge.shared.actions.Action
import risk.shared.abstract0.{
  DiceResultForAttacker,
  DiceResultForDefender,
  DivideTerritoriesBetweenPlayers
}
import zio.{UIO, ZIO, durationInt}

object RiskEffects {
  val dtbp: UIO[Option[Action]] = for {
    seed <- zio.Random.nextLong.map(_.abs)
  } yield Some(DivideTerritoriesBetweenPlayers(seed))

  def attackerDiceEffect(noOfAttackers: Int): UIO[Option[Action]] = for {
    dices <- ZIO.foreach((1 to noOfAttackers).toList)(_ =>
      zio.Random.nextIntBounded(6).map(_ + 1)
    )
    _ <- ZIO.sleep(2.seconds)
  } yield Some(DiceResultForAttacker(dices))

  def defenderDiceEffect(noOfDefenders: Int): UIO[Option[Action]] = for {
    dices <- ZIO.foreach((1 to noOfDefenders).toList)(_ =>
      zio.Random.nextIntBounded(6).map(_ + 1)
    )
    _ <- ZIO.sleep(2.seconds)
  } yield Some(DiceResultForDefender(dices))
}
