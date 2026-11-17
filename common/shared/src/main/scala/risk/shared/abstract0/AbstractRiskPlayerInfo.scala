package risk.shared.abstract0

abstract class AbstractRiskPlayerInfo(
    val troops: Int,
    val eliminated: Boolean = false,
    val rollDefenderDieAutomatically: Boolean = true,
    val rollAttackerDieAutomatically: Boolean = true,
    val defendWithOneAutomaticallyIfOnlyOneUnitIsPresent: Boolean = true
)
