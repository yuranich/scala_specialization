package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      elem <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(elem, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h1 = insert(b, h)
    findMin(h1) == Math.min(a, b)
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meldMin") = forAll { (h1: H, h2: H) =>
    val min1 = if (isEmpty(h1)) Int.MaxValue else findMin(h1)
    val min2 = if (isEmpty(h2)) Int.MaxValue else findMin(h2)
    if (isEmpty(h1) && isEmpty(h2)) isEmpty(meld(h1, h2))
    else findMin(meld(h1, h2)) == Math.min(min1, min2)
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimum.
  property("sorting") = forAll { h: H =>
    val ascList = toAscList(h, List[Int]())
    val fallen = for (i <- 0 until ascList.length - 1 if ascList(i) > ascList(i+1)) yield false
    fallen.isEmpty
  }

  //Given any heap, sorted sequence of it with max integer appended should be the same
  //as sorted sequence of heap with inserted max integer.
  property("insMaxVal") = forAll { h: H =>
    val h2 = insert(Int.MaxValue, h)
    val hList = toAscList(h, List())
    val h2List = toAscList(h2, List())
    hList :+ Int.MaxValue == h2List
  }

  //sorted sequence of a heap shouldn't be broken after deletion, insertion and melding with another heap.
  property("meldAfterMove") = forAll { (h1: H, h2: H) =>
    val h = if (isEmpty(h1)) insert(0, empty) else h1
    val min1 = findMin(h)
    val h1dash = deleteMin(h)
    val h2dash = insert(min1, h2)
    val meld1 = meld(h1dash, h2dash)
    val ascList = toAscList(meld1, List[Int]())
    val fallen = for (i <- 0 until ascList.length - 1 if ascList(i) > ascList(i+1)) yield false
    fallen.isEmpty
  }

  //helper method for getting sorted sequence from heap.
  def toAscList(heap: H, acc: List[Int]): List[Int] = {
    if (isEmpty(heap)) acc
    else {
      val curMin = findMin(heap)
      toAscList(deleteMin(heap), acc :+ curMin)
    }
  }
}
