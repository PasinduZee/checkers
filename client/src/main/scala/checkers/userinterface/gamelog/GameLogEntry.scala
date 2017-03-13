package checkers.userinterface.gamelog

import checkers.consts._
import checkers.userinterface.mixins.{ClipPathHelpers, FontHelpers}
import checkers.userinterface.piece.{PhysicalPiece, PhysicalPieceProps}
import checkers.util.CssHelpers
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object GameLogEntry {

  case class Props(widthPixels: Double,
                   heightPixels: Double,
                   turnIndex: Int,
                   side: Side,
                   moveDescription: Option[String],
                   upperLeftX: Double = 0,
                   upperLeftY: Double = 0)

  private def getStripeClass(props: Props): String =
    if (props.turnIndex % 2 == 0) "even" else "odd"
}

class GameLogEntry(physicalPiece: PhysicalPiece) extends FontHelpers with ClipPathHelpers {

  import GameLogEntry._

  class Backend($: BackendScope[Props, Unit]) {

    def render(props: Props): VdomElement = {
      val stripe = getStripeClass(props)
      val textHeight = 0.46 * props.heightPixels

      val fontSizePixels = textHeightPixels(textHeight)

      val textBottom = 0.7 * props.heightPixels
      val turnIndexLeft = 0.05 * props.widthPixels
      val turnIndexRight = 0.23 * props.widthPixels
      val descriptionLeft = 0.44 * props.widthPixels
      val descriptionRight = 0.95 * props.widthPixels

      val halfHeight = 0.5 * props.heightPixels
      val avatarX = (turnIndexRight + descriptionLeft) / 2
      val avatarScale = math.min(0.92 * props.heightPixels, 0.92 * (descriptionLeft - turnIndexRight))

      val backdrop = <.svg.rect(
        ^.key := "backdrop",
        ^.`class` := s"game-log-entry-backdrop $stripe ${CssHelpers.playerSideClass(props.side)}",
        ^.svg.x := 0,
        ^.svg.y := 0,
        ^.svg.width := props.widthPixels,
        ^.svg.height := props.heightPixels
      )

      val turnIndexCaption = s"${props.turnIndex}."

      val turnIndexFontSize = if(props.turnIndex >= 100) {
        val newHeight = 0.8 * textHeight
        textHeightPixels(newHeight)
      } else fontSizePixels

      val turnIndexLabel = <.svg.text(
        ^.`class` := "turn-index",
        ^.svg.x := turnIndexRight,
        ^.svg.y := textBottom,
        ^.svg.textAnchor := "end",
        fontSize := turnIndexFontSize,
        turnIndexCaption
      )

      val descriptionLabel = props.moveDescription.map { caption =>
        <.svg.text(
          ^.`class` := "description",
          ^.svg.x := descriptionLeft,
          ^.svg.y := textBottom,
          ^.svg.textAnchor := "begin",
          fontSize := fontSizePixels,
          caption
        )
      }


      val clipPathId = s"game-log-clip-path-${props.turnIndex}"

      val textClipPath = <.svg.defs(
        <.svg.clipPathTag(
          ^.id := clipPathId,
          <.svg.rect(
            ^.svg.x := turnIndexLeft,
            ^.svg.y := 0,
            ^.svg.width := descriptionRight - turnIndexLeft,
            ^.svg.height := props.heightPixels
          )
        )
      )

      val avatar = {
        val piece = MAKEMAN(props.side)
        val avatarProps = PhysicalPieceProps.default.copy(
          piece = piece,
          x = avatarX,
          y = halfHeight,
          scale = avatarScale,
          simplified = true)
        physicalPiece.create(avatarProps)
      }

      val textElements = <.svg.g(
        clipPathAttr := s"url(#$clipPathId)",
        turnIndexLabel,
        descriptionLabel
      )

      val transform = s"translate(${props.upperLeftX},${props.upperLeftY})"

      <.svg.g(
        ^.`class` := "game-log-entry",
        ^.svg.transform := transform,
        textClipPath,
        backdrop,
        textElements,
        avatar
      )
    }
  }

  val create = ScalaComponent.build[Props]("GameLogEntry")
    .renderBackend[Backend]
    .shouldComponentUpdateConst { case ShouldComponentUpdate(scope, nextProps, _) =>
      val result = scope.props != nextProps
      CallbackTo.pure(result)
    }
    .build
}