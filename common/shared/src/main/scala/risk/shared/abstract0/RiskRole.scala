package risk.shared.abstract0

import gbge.shared.GameRole

sealed trait RiskRole extends GameRole {
  val color: RiskColor
}

object RiskRole {
  case object Red extends RiskRole {
    override val roleId: Int = 1
    override val color: RiskColor = RiskColor.Red
  }

  case object Green extends RiskRole {
    override val roleId: Int = 2
    override val color: RiskColor = RiskColor.Green
  }

  case object Blue extends RiskRole {
    override val roleId: Int = 3
    override val color: RiskColor = RiskColor.Blue
  }

  case object White extends RiskRole {
    override val roleId: Int = 4
    override val color: RiskColor = RiskColor.White
  }

  case object Yellow extends RiskRole {
    override val roleId: Int = 5
    override val color: RiskColor = RiskColor.Yellow
  }

  val allRole: Set[RiskRole] = Set(Red, Green, Blue, White, Yellow)

  def getRoleFromRoleId(roleId: Int): RiskRole =
    roleId match {
      case 1 => Red
      case 2 => Green
      case 3 => Blue
      case 4 => White
      case 5 => Yellow
      case _ =>
        throw new IllegalArgumentException(s"Unknown player role id: $roleId")
    }
}
