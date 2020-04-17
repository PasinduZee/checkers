package checkers.userinterface.leaderboard

import checkers.consts._
import checkers.core.Variation
import checkers.util.StringUtils
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{svg_<^ => svg}

object LeaderBoard {

  sealed trait LeaderBoardResult

  case object Ok extends LeaderBoardResult

  trait LeaderBoardCallbacks {
    def onLeaderBoardResult(result: LeaderBoardResult): Callback
  }

  case class State(graphMode: Int) {
  }

  case class Props(learderBoardMode: Int,
                   callbacks: LeaderBoardCallbacks) {
    def initialState: State = State(learderBoardMode)
  }
}

class LeaderBoard() {

  import LeaderBoard._

  trait DialogButtonsCallbacks {
    def onOkClicked: Callback
  }

  class DialogButtonsBackend($: BackendScope[DialogButtonsCallbacks, Unit]) {
    def render(callback: DialogButtonsCallbacks): VdomElement = {
      <.div(
        <.button(
          ^.onClick --> callback.onOkClicked,
          "OK"
        )
      )
    }
  }

  private val DialogButtons = ScalaComponent.builder[DialogButtonsCallbacks]("DialogButtons")
    .renderBackend[DialogButtonsBackend]
    .build


  class LeaderBoardBackend($: BackendScope[Props, State]) extends DialogButtonsCallbacks {

    def render(props: Props, state: State): VdomElement = {

      val dialogButtons = DialogButtons(this)

      <.div(
        ^.id := "new-game-dialog",
        ^.`class` := "modal-dialog",
        <.div(
          ^.`class` := "modal-background",
          <.div(
            ^.`class` := "modal-content",
            <.div(
              ^.`class` := "modal-header",
              <.span(
                ^.`class` := "close",
                ^.onClick --> onOkClicked,
                "×"
              ),
              <.h2("Leaderboard")
            ),
            <.div(
              ^.`class` := "modal-body",
            ),
            <.div(
              ^.`class` := "modal-footer",
              dialogButtons
            )
          )
        )
      )
    }

    def onOkClicked: Callback = for {
      props <- $.props
      state <- $.state
      data = Ok
      LeaderBoardResult <- props.callbacks.onLeaderBoardResult(data)
    } yield LeaderBoardResult
  }

  val create = ScalaComponent.builder[Props]("LeaderBoard")
    .initialStateFromProps[State](_.initialState)
    .renderBackend[LeaderBoardBackend]
    .build

}