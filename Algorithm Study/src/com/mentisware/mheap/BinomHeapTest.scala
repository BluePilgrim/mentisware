package com.mentisware.mheap

import com.mentisware.test.UnitSpec

/**
 * @author David Yang
 */

trait BinomHeapTestSet {
  def testSet1 =
    List(3606, 5553, 9217, 9804, 750, 5359, 1044, 1818, 9789, 2844)
//    List(4, 1, 3, 2, 5, 10, -2)

  def testSet2(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    List.fill(size)(rnd.nextInt(10000))
  }
}

class BinomHeapTest extends UnitSpec with MergeableHeapBehavior with BinomHeapTestSet {
  val set1 = testSet1
  "Determined Set with Binomial Heap" should behave like setupCorrectly(set1)(BinomHeap.apply[Int])
  it should behave like performMHeapOperations(set1)(BinomHeap.apply[Int])

  val set2 = testSet2(10000)
//  println("rnd set = " + set2)
  "Random Set with Binomial Heap" should behave like setupCorrectly(set2)(BinomHeap.apply[Int])
  it should behave like performMHeapOperations(set2)(BinomHeap.apply[Int])

}