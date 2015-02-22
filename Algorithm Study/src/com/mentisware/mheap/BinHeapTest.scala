package com.mentisware.mheap

import com.mentisware.test.UnitSpec

trait BinHeapTestSet {
  def testSet1 =
    List(4, 1, 3, 2, 5, 10, -2)

  def testSet2(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    List.fill(size)(rnd.nextInt(10000))
  }
}

class BinHeapTest extends UnitSpec with MergeableHeapBehavior with BinHeapTestSet {
  val set1 = testSet1
  "Determined Set with Binary Heap" should behave like setupCorrectly(set1)(BinHeap.apply[Int])
  it should behave like performMHeapOperations(set1)(BinHeap.apply[Int])

  val set2 = testSet2(10000)
  "Random Set with Binary Heap" should behave like setupCorrectly(set2)(BinHeap.apply[Int])
  it should behave like performMHeapOperations(set2)(BinHeap.apply[Int])

}
