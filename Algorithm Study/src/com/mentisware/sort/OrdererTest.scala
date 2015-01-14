package com.mentisware.sort

import com.mentisware.test.UnitSpec

class OrdererTest extends UnitSpec {
  def testSet1 = (List(3, 4, 1), 2 , 3)
  def testSet2 = (List(3, 4, 1, 6, -1, 2, 4, 2, 10, -4), 5 , 2)
  def testSet3 = {
    val xs, ws = List(0.1, 0.35, 0.05, 0.1, 0.15, 0.05, 0.2)
    (xs, ws, 0.1, 0.2)
  }

  def randomSet1(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs = new scala.collection.mutable.ListBuffer[Int]

    def createRandomList(count: Int) {
      if (count > 0) {
        (rnd.nextInt(10000)) +=: xs
        createRandomList(count - 1)
      }
    }

    createRandomList(size)
    xs.toList
  }

  "O(n) selection of kth smallest" should "perform correctly" in {
    val (xs, k, v) = testSet2
    (Orderer.selectSmallest(xs, k)) should equal (v)
  }
  
  it should "return the same value as indexing after sort" in {
    val testSet = randomSet1(1000)
    val (k1, k2) = (300, 678)
    val sortedSet = Sorter.mergeSort(testSet).toArray
    
    (Orderer.selectSmallest(testSet, k1)) should equal (sortedSet(k1 - 1))
    (Orderer.selectSmallest(testSet, k2)) should equal (sortedSet(k2 - 1))
  }
  
  
  "O(n) selection of k% portion of weighted sum" should
    "return the same result with selection of kth number with uniform weights" in {
    val (xs, k, v) = testSet2
    val len = xs.length
    val ws = List.fill(len)(1.0)
    
    (Orderer.selectWeightedSum(xs, ws, k * 100 / xs.length)) should equal (Some(v))
  }
  
  it should "return the same result as normal selection with uniform weights for random set" in {
    val testSet = randomSet1(1000)
    val ws = List.fill(testSet.size)(10.0)
    val (k1, k2) = (579, 821)
    
    (Some(Orderer.selectSmallest(testSet, k1))) should equal (Orderer.selectWeightedSum(testSet, ws, 100.0 * k1 / testSet.size))
    (Some(Orderer.selectSmallest(testSet, k2))) should equal (Orderer.selectWeightedSum(testSet, ws, 100.0 * k2 / testSet.size))    
  }
  
  it should "return the unweighted media and the weight media correctly" in {
    val (xs, ws, m, wm) = testSet3
    (Orderer.selectWeightedSum(xs, List.fill(xs.length)(1.0), 50)) should equal (Some(m))
    (Orderer.selectWeightedSum(xs, ws, 50)) should equal (Some(wm))
  }
  
  it should "return None for 0%" in {
    val (xs, k, v) = testSet2
    val len = xs.length
    val ws = List.fill(len)(1.0)

    (Orderer.selectWeightedSum(xs, ws, 0)) should be (None)
  }
}