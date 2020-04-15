package checkers.userinterface.leaderboard

import checkers.consts._
import checkers.core.Variation
import checkers.util.StringUtils
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{svg_<^ => svg}


object LeaderBoard {

  sealed trait Result

  case object Ok extends Result

  trait LeaderBoardCallbacks {
    def onLeaderBoardResult(result: Result): Callback
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


  class LeaderBoardBackend($: BackendScope[Unit]) extends DialogButtonsCallbacks{

    def render(): VdomElement = {

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
              <.h2("New Game")
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
      state <- $.state
      data = Ok
      result <- props.callbacks.onLeaderBoardResult(data)
    } yield result
  }

  val create = ScalaComponent.builder[Unit]("LeaderBoard")
    .renderBackend[LeaderBoardBackend]
    .build

}