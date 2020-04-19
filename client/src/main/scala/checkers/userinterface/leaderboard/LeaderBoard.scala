package checkers.userinterface.leaderboard

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._


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

  trait LeaderBoardTableCallbacks {
  }


  private val DialogButtons = ScalaComponent.builder[DialogButtonsCallbacks]("DialogButtons")
    .renderBackend[DialogButtonsBackend]
    .build

  private val LeaderBoardTable = ScalaComponent.builder[LeaderBoardTableProps]("LeaderBoardTable")
    .renderBackend[LeaderBoardTableBackend]
    .build


  case class Deposit(period: Int, interest: Double) {
    def gain(amount: Double, duration: Int) = {
      val interestPerPeriod = interest * (period / 12.0)
      val fullPeriods       = duration / period
      val finalBalance      = amount * math.pow(1 + interestPerPeriod, fullPeriods)

      finalBalance - amount
    }
  }

  val amount   : Double   = 10000.0
  val duration : Int     = 12
  val deposits = Map(1-> Deposit(1,0.02), 2 -> Deposit(3,0.025), 3 -> Deposit(12,0.03))

  case class LeaderBoardTableProps (
    rowCount: Int,
    entries: Map[Int, Deposit]
  )

  class LeaderBoardTableBackend($: BackendScope[LeaderBoardTableProps, Unit]) {
    def render(props: LeaderBoardTableProps): VdomElement = {
      <.table(
        <.thead(
          <.tr(
            <.th( "Period (months)" ),
            <.th( "Interest Rate (%/year)" ),
            <.th( "Gain ($)"),
          ),
        ),
        deposits.map {
          case (depositId,depositValue) => this.renderDeposit(depositValue, amount, duration)
        }.toTagMod
      ),
    }

    def renderDeposit(deposit: Deposit, amount: Double, duration: Int): TagMod= {
      <.tr(
        <.td( deposit.period.toString ),
        <.td((deposit.interest * 100).toString ),
        <.td(
          {
            val gain = deposit.gain(amount, duration)
            f"${gain}%.2f"
          }
        ),
      )
    }

  }

  class LeaderBoardBackend($: BackendScope[Props, State]) extends DialogButtonsCallbacks  {

    def render(props: Props, state: State): VdomElement = {

      val tableProperties = LeaderBoardTableProps(
        rowCount = 10,
        entries = deposits)

      val leaderBoardTable = LeaderBoardTable(tableProperties)
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
              leaderBoardTable,
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