package risk.shared.abstract0

import upickle.default.{macroRW, ReadWriter => RW}

case class ColorMap(colorMap: Map[RiskColor, Int] = Map.empty) {

  def removeColor(riskColor: RiskColor): ColorMap = {
    if (colorMap.contains(riskColor)) {
      ColorMap(colorMap.filterNot(_._1 == riskColor))
    } else this
  }

  def addOrUpdateColor(tuple: (RiskColor, Int)): ColorMap = {
    val (color, roleId) = tuple
    val cor = colorOfRole(roleId)
    if (cor.isEmpty)
      ColorMap(colorMap + (color -> roleId))
    else {
      val newColorMap = (colorMap - cor.get) + (color -> roleId)
      ColorMap(newColorMap)
    }

  }
  def roleOfColor(color: RiskColor): Option[Int] = {
    colorMap.get(color)
  }
  def colorOfRole(role: Int): Option[RiskColor] = {
    colorMap.map(_.swap).get(role)
  }

  val size: Int = colorMap.size

  val isEmpty: Boolean = colorMap.isEmpty
}

object ColorMap {
  implicit def rw: RW[ColorMap] = macroRW
}
