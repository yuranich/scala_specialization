package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    countOpen(chars, 0, 0, chars.length) == 0
  }

  def countOpen(chars: Array[Char], startOpen: Int, from: Int, until: Int): Int = {
    if (until - from < 1) return 0
    var next = from
    var open = startOpen
    while (next < until) {
      val cur = chars(next)
      if (cur == ')') {
        if (open < 1) {
          return -1
        } else {
          open -= 1
        }
      } else if (cur == '(') {
        open += 1
      } else {
        //nothing to do here
      }
      next += 1
    }
    open
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, accO: Int, accC: Int): (Int, Int) = {
      if (idx >= until) return (accO, accC)
      val next = chars(idx)
      if (next == ')') {
        traverse(idx + 1, until, accO, accC + 1)
      } else if (next == '(') {
        traverse(idx + 1, until, accO + 1, accC)
      } else {
        traverse(idx + 1, until, accO, accC)
      }
    }

    def reduce(from: Int, until: Int): Int = {
      sealed trait TreeResInt { val res: Int }
      case class Leaf (from: Int, end: Int, override val res: Int) extends TreeResInt
      case class Node (l: TreeResInt, override val res: Int, r: TreeResInt) extends TreeResInt

      def upsweep(from: Int, until: Int): TreeResInt = {
        if (until - from <= threshold) {
          val accTup = traverse(from, until, 0, 0)
          Leaf(from, until, accTup._1 - accTup._2)
        } else {
          val middle = (until + from) / 2
          val (left, right) = parallel(upsweep(from, middle), upsweep(middle, until))
          Node(left, left.res + right.res, right)
        }
      }

      def downSweep(tree: TreeResInt, a0: Int): Int = tree match {
        case Leaf(start, end, v) => if (a0 < 0) -1 else countOpen(chars, a0, start, end)
        case Node(l, v, r) =>
          val (left, right) = parallel(downSweep(l, a0), downSweep(r, a0 + l.res))
          if (left < 0 || right < 0) -1 else 0
      }

      val resTree = upsweep(from, until)

      if (resTree.res != 0) -1
      else downSweep(resTree, 0)
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
