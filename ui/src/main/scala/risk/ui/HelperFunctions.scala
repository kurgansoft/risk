package risk.ui

object HelperFunctions {

  val fraction = "-?\\d+(\\.\\d+)?"
  val moveCommand = "M" + fraction + "," + fraction
  val curve = "(c|C)" + fraction + ",?" + fraction + ",?" + fraction + ",?" + fraction + ",?" + fraction + ",?" + fraction
  val capitalCurve = "C" + fraction + ",?" + fraction + ",?" + fraction + ",?" + fraction + ",?" + fraction + ",?" + fraction

  def calculateSmallestBoundingRectangleOfSvgPath(svgPath: String): (BigDecimal,BigDecimal,BigDecimal,BigDecimal) = {
    var xCoordinates: List[BigDecimal] = List.empty
    var yCoordinates: List[BigDecimal] = List.empty

    moveCommand.r.findAllIn(svgPath).foreach(pattern => {
      var odd: Boolean = true
      fraction.r.findAllIn(pattern).foreach(number => {
        val asDecimal = BigDecimal(number)
        if (odd)
          xCoordinates = asDecimal::xCoordinates
        else
          yCoordinates = asDecimal::yCoordinates
        odd= !odd
      })
    })

    capitalCurve.r.findAllIn(svgPath).foreach(pattern => {
      var odd: Boolean = true
      fraction.r.findAllIn(pattern).foreach(number => {
        val asDecimal = BigDecimal(number)
        if (odd)
          xCoordinates = asDecimal::xCoordinates
        else
          yCoordinates = asDecimal::yCoordinates
        odd= !odd
      })
    })
    (xCoordinates.min, yCoordinates.min, xCoordinates.max, yCoordinates.max)
  }

  def pushSvgPath(svgPath: String, newPole: (BigDecimal, BigDecimal)): String = {
    val step1 = moveCommand.r.replaceAllIn(svgPath, mc => {
      var odd = true
      fraction.r.replaceAllIn(mc.toString(), fr => {
        val x = BigDecimal(fr.toString())
        val v7 = if (odd) {
          x - newPole._1
        } else {
          x - newPole._2
        }
        if (v7 < 0) println(s"strange: $v7")
        odd = !odd
        v7.toString
      })
    })

    val step2 = capitalCurve.r.replaceAllIn(step1, mc => {
      var odd = false
      fraction.r.replaceAllIn(mc.toString(), fr => {
        odd = !odd
        val x = BigDecimal(fr.toString())
        if (odd) {
          (x - newPole._1).toString()
        } else {
          (x - newPole._2).toString()
        }
      })
    })
    step2
  }
}
