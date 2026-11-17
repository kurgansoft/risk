package risk.shared.abstract0

sealed trait RiskColor

object RiskColor {

  case object Red extends RiskColor {
    override def toString: String = "red"
  }

  case object Green extends RiskColor {
    override def toString: String = "green"
  }

  case object Blue extends RiskColor {
    override def toString: String = "blue"
  }

  case object White extends RiskColor {
    override def toString: String = "white"
  }

  case object Yellow extends RiskColor {
    override def toString: String = "yellow"
  }

  val enumerateColors: List[RiskColor] = List(Red, White, Green, Blue, Yellow)
}
