package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (is: (Int, Int)) =>
    val min1 = is._1
    val min2 = is._2
    val h = insert(min1, insert(min2, empty))
    findMin(h) == (if (min1 > min2) min2 else min1)
  }

  def getSortedList(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else
      findMin(h) :: getSortedList(deleteMin(h))
  }

  property("deleteMin1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("deleteMin2") = forAll { (h: H, i: Int) =>
    val min = findMin(h)
    val minimal = if (min < i) min else i
    val h1 = insert(minimal, h)
    deleteMin(h1) == h
  }

  property("delete 3") = forAll { (l: List[Int], h: H) =>
    val h = l.foldLeft(empty)((acc, n) => insert(n, acc))
    getSortedList(h) == l.sorted
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    findMin(meld(h1, h2)) == (if (min1 > min2) min2 else min1)
  }
}
