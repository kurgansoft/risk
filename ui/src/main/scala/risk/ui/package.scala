package risk

import risk.shared.abstract0
import risk.shared.abstract0.RiskColor

package object ui {
  extension (riskColor: abstract0.RiskColor)
    inline def colorCode: String = riskColor match {
      case RiskColor.Red    => "#d62828"
      case RiskColor.Green  => "#2e8b57"
      case RiskColor.Blue   => "#1f5aa6"
      case RiskColor.White  => "#f5f5f5"
      case RiskColor.Yellow => "#f4d03f"
    }

    inline def textColorCode: String = riskColor match {
      case RiskColor.Red | RiskColor.Blue                       => "white"
      case RiskColor.Green | RiskColor.White | RiskColor.Yellow => "#243247"
    }

    inline def contrastColorCode: String = riskColor match {
      case RiskColor.White | RiskColor.Yellow => "black"
      case _                                  => "yellow"
    }
}
