package checkers.computer

import checkers.consts._
import checkers.core._
import checkers.core.tables.NeighborIndex
import checkers.test.BoardUtils.BoardStats
import checkers.test.generators.{BoardGenerators, ColorGenerator}
import checkers.test.{BoardUtils, DefaultGameLogicTestModule, TestSuiteBase}
import com.softwaremill.macwire._
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._
import utest.framework._

object EvaluatorTests extends TestSuiteBase
  with DefaultGameLogicTestModule
  with ColorGenerator
  with BoardGenerators {

  lazy val evaluator = wire[DefaultEvaluator]
  lazy val moveGenerator = gameLogicModule.moveGenerator
  lazy val moveDecoder = new MoveDecoder
  lazy val jumpTable = tablesModule.jumpTable
  lazy val neighborTable = tablesModule.neighborTable
  lazy val innerSquares: Set[Int] = BoardUtils.squareMaskToSet(checkers.masks.INNER)

  private def getMoveList(boardStack: BoardStack, turnToMove: Color): List[List[Int]] = {
    val moveList = moveGenerator.generateMoves(boardStack, turnToMove)
    moveDecoder.allPaths(moveList)
  }

  case class ExpectedAttacks(dark: Set[Int], light: Set[Int])

  private def isColor(color: Color, occupant: Occupant): Boolean = ISPIECE(occupant) && COLOR(occupant) == color

  private def getExpectedAttacks(board: BoardStateRead): ExpectedAttacks = {
    var dark = Set.empty[Int]
    var light = Set.empty[Int]

    def evaluateForColor(color: Color, king: Occupant, neighbors: NeighborIndex): Set[Int] = {
      var result = Set.empty[Int]
      innerSquares.foreach { idx =>
        val isAttack = board.isSquareEmpty(idx) && {
          val forwardW = board.getOccupant(neighbors.forwardMoveW(idx))
          val forwardE = board.getOccupant(neighbors.forwardMoveE(idx))
          val backW = board.getOccupant(neighbors.backMoveW(idx))
          val backE = board.getOccupant(neighbors.backMoveE(idx))

          (isColor(color, backW) && !isColor(color, forwardE)) ||
            (isColor(color, backE) && !isColor(color, forwardW)) ||
            (!isColor(color, backW) && forwardE == king) ||
            (!isColor(color, backE) && forwardW == king)
        }
        if (isAttack) result += idx
      }
      result
    }

    ExpectedAttacks(dark = evaluateForColor(DARK, DARKKING, neighborTable.Dark),
      light = evaluateForColor(LIGHT, LIGHTKING, neighborTable.Light))
  }


  case class ProbeDataSide(manCount: Int,
                           kingCount: Int,
                           attackMask: Int,
                           safeMask: Int,
                           canEscapeMask: Int,
                           trappedKingMask: Int,
                           attackSet: Set[Int],
                           trappedKingSet: Set[Int])

  case class ProbeData(dark: ProbeDataSide,
                       light: ProbeDataSide,
                       potentialAttackMask: Int)

  object ProbeData {
    def fromTestProbe(input: DefaultEvaluatorTestProbe): ProbeData = {
      val dark = ProbeDataSide(input.darkManCount,
        input.darkKingCount,
        input.darkAttackMask,
        input.darkSafeMask,
        input.darkCanEscapeMask,
        input.darkTrappedKingMask,
        attackSet = BoardUtils.squareMaskToSet(input.darkAttackMask),
        trappedKingSet = BoardUtils.squareMaskToSet(input.darkTrappedKingMask))

      val light = ProbeDataSide(input.lightManCount,
        input.lightKingCount,
        input.lightAttackMask,
        input.lightSafeMask,
        input.lightCanEscapeMask,
        input.lightTrappedKingMask,
        attackSet = BoardUtils.squareMaskToSet(input.lightAttackMask),
        trappedKingSet = BoardUtils.squareMaskToSet(input.lightTrappedKingMask))

      ProbeData(dark, light, input.potentialAttackMask)
    }
  }

  case class EvaluatorPropInput(board: BoardState,
                                turnToMove: Color,
                                probeData: ProbeData,
                                darkMoves: List[List[Int]],
                                lightMoves: List[List[Int]],
                                expectedAttacks: ExpectedAttacks,
                                boardStats: BoardStats,
                                evaluationResult: Int)

  lazy val genEvaluatorPropInput: Gen[EvaluatorPropInput] = for {
    turnToMove <- genColor
    board <- genBoard
  } yield {
    val boardStack = BoardStack.fromBoard(board)
    val darkMoves = getMoveList(boardStack, DARK)
    val lightMoves = getMoveList(boardStack, LIGHT)
    val expectedAttacks = getExpectedAttacks(board)
    val boardStats = BoardUtils.getBoardStats(board)
    val testProbe = new DefaultEvaluatorTestProbe
    val evaluationResult = evaluator.evaluate(turnToMove, board, testProbe)
    val probeData = ProbeData.fromTestProbe(testProbe)
    EvaluatorPropInput(board, turnToMove, probeData, darkMoves, lightMoves, expectedAttacks, boardStats, evaluationResult)
  }

  private def testProbeCheck(name: String, f: (ProbeData, EvaluatorPropInput) => Boolean): Prop[EvaluatorPropInput] =
    Prop.test(name, { input => f(input.probeData, input) })

  lazy val darkManCount = testProbeCheck("darkManCount", _.dark.manCount == _.boardStats.darkMan)
  lazy val lightManCount = testProbeCheck("lightManCount", _.light.manCount == _.boardStats.lightMan)
  lazy val darkKingCount = testProbeCheck("darkKingCount", _.dark.kingCount == _.boardStats.darkKing)
  lazy val lightKingCount = testProbeCheck("lightKingCount", _.light.kingCount == _.boardStats.lightKing)

  lazy val darkAttacks = testProbeCheck("darkAttacks", _.dark.attackSet == _.expectedAttacks.dark)
  lazy val lightAttacks = testProbeCheck("lightAttacks", _.light.attackSet == _.expectedAttacks.light)

  lazy val evaluatorPropInputProps = darkManCount & lightManCount & darkKingCount & lightKingCount &
    darkAttacks & lightAttacks


  override def tests: Tree[Test] = TestSuite {
    'Evaluator {
      genEvaluatorPropInput.mustSatisfy(evaluatorPropInputProps)
    }

  }

}