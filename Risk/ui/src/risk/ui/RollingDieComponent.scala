package risk.ui

import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.all._
import scala.scalajs.js
import scala.util.Random

object RollingDieComponent {

  case class State(currentRandomDie: List[Int])

  class Backend($: BackendScope[Int, State]) {

    var interval: js.UndefOr[js.timers.SetIntervalHandle] =
      js.undefined

    def tick = $.modState(_ => {
      val numbers: List[Int] =
        (for (_ <- 1 to $.props.runNow())
          yield Random.between(1,7)).toList
      State(numbers)
    })

    def start = Callback {
      $.modState(_ => State(List.empty))
      tick.runNow()
      interval = js.timers.setInterval(200)(tick.runNow())
    }

    def stop = Callback {
      interval foreach js.timers.clearInterval
      interval = js.undefined
    }

    def render(s: State) = {
      div(display:="flex", flexDirection:="column",
        s.currentRandomDie.map(n => div(Directives.integerToDieString(n))).toTagMod
      )
    }
  }

  val rollingDieComponent = ScalaComponent.builder[Int]
    .initialState(State(List.empty))
    .renderBackend[Backend]
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.stop)
    .build

}
