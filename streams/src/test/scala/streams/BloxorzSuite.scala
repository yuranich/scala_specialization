package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /** This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
      //println(solution)
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  //My tests

  test("neighbors with history"){
    new Level1 {
      val nwh = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      //nwh.foreach(item => println(item))
      assert((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)) === nwh.head)
      assert((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)) === nwh.tail.head)
    }
  }
  test("new Neighbors"){
    new Level1 {
      val neighbors = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
      val visited = Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      assert(
        newNeighborsOnly(neighbors, visited)
          ===
          Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  trait Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  test("test parsing"){
    new Level0 {
      assert(Pos(1, 2) === startPos)
      assert(Pos(1, 3) === goal)

      assert(!terrain(Pos(0, 0)), "0,  0")
      assert(!terrain(Pos(1, 0)), "1,  0")
      assert(!terrain(Pos(2, 0)), "2,  0")
      assert(!terrain(Pos(3, 0)), "3,  0")
      assert(!terrain(Pos(4, 0)), "4,  0")

      assert(!terrain(Pos(0, 1)), "0,  1")
      assert(!terrain(Pos(1, 1)), "1,  1")
      assert(!terrain(Pos(2, 1)), "2,  1")
      assert(!terrain(Pos(3, 1)), "3,  1")
      assert(!terrain(Pos(4, 1)), "4,  1")

      assert(!terrain(Pos(0, 4)), "0,  4")
      assert(!terrain(Pos(1, 4)), "1,  4")
      assert(!terrain(Pos(2, 4)), "2,  4")
      assert(!terrain(Pos(3, 4)), "3,  4")
      assert(!terrain(Pos(4, 4)), "4,  4")

      assert(!terrain(Pos(0, 5)), "0,  5")
      assert(!terrain(Pos(1, 5)), "1,  5")
      assert(!terrain(Pos(2, 5)), "2,  5")
      assert(!terrain(Pos(3, 5)), "3,  5")
      assert(!terrain(Pos(4, 5)), "4,  5")

      assert(!terrain(Pos(0, 2)), "0,  2")
      assert(terrain(Pos(1, 2)), "1,  2")
      assert(terrain(Pos(2, 2)), "2,  2")
      assert(terrain(Pos(3, 2)), "3,  2")
      assert(!terrain(Pos(4, 2)), "4,  2")

      assert(!terrain(Pos(0, 3)), "0,  3")
      assert(terrain(Pos(1, 3)), "1,  3")
      assert(terrain(Pos(2, 3)), "2,  3")
      assert(terrain(Pos(3, 3)), "3,  3")
      assert(!terrain(Pos(4, 3)), "4,  3")
    }
  }

  trait Level3 extends SolutionChecker {
    val level =
      """------ooooooo--
        |oooo--ooo--oo--
        |ooooooooo--oooo
        |oSoo-------ooTo
        |oooo-------oooo
        |------------ooo""".stripMargin
  }
  test("optimal solution for level 3") {
    new Level3 {
      println("Paths from start...")
      pathsFromStart.take(5).foreach(println(_))
      println()
      println("Paths to goal...")
      pathsToGoal.take(5).foreach(println(_))
      println()
      println("Solution...")
      println(solution)
    }
  }

  trait Level4 extends SolutionChecker {
    val level =
      """---ooooooo----
        |---ooooooo----
        |oooo-----ooo--
        |ooo-------oo--
        |ooo-------oo--
        |oSo--ooooooooo
        |ooo--ooooooooo
        |-----oTo--oooo
        |-----ooo--oooo""".stripMargin
  }
  test("optimal solution for level 4") {
    new Level4 {
      println("Paths from start...")
      pathsFromStart.take(5).foreach(println(_))
      println()
      println("Paths to goal...")
      pathsToGoal.take(5).foreach(println(_))
      println()
      println("Solution...")
      println(solution)
    }
  }

  trait LevelBAD extends SolutionChecker {
    val level =
      """---ooooooo----
        |---ooooooo----
        |oooo----------
        |ooo-----------
        |ooo-----------
        |oSo--ooooooooo
        |ooo--ooooooooo
        |-----oTo--oooo
        |-----ooo--oooo""".stripMargin
  }
  test("no solution possible") {
    new LevelBAD {
      println("Paths from start...")
      pathsFromStart.take(5).foreach(println(_))
      println()
      println("Paths to goal...")
      pathsToGoal.take(5).foreach(println(_))
      println()
      println("Solution...")
      println(solution)
    }
  }

}