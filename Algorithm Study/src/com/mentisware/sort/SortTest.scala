package com.mentisware.sort

import com.mentisware.test.UnitSpec


trait SortBehaviors[T] { this: SortTest =>
  def comparisonSort(sortF: List[T] => List[T])(testSets: (List[T], List[T])*) {
    it should "have the same result with the reference" in {
      testSets foreach { testSet =>
        (sortF(testSet._1)) should equal (testSet._2)
      }
    }
  }
  
  def linearTimeSort(sortF: (List[T], Int) => List[T])(testSets: (List[T], Int, List[T])*) {
    it should "have the same result with the reference" in {
      testSets foreach { testSet =>
        (sortF(testSet._1, testSet._2)) should equal (testSet._3)
      }
    }
  }
}

class SortTest extends UnitSpec with SortBehaviors[Int] {
  def testSet1 = List(24, 1, 23, -1, 2, 3, 4, 1)
  
  def testSet2 = List(2, 1, 2, 1, 4, 3)
  
  def randomSet1 = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs = new scala.collection.mutable.ListBuffer[Int]

    def createRandomList(count: Int) {
      if (count > 0) {
        (rnd.nextInt(10000)) +=: xs
        createRandomList(count - 1)
      }
    }

    createRandomList(1000)
    xs.toList    
  }
  
  def randomSet2 = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs = new scala.collection.mutable.ListBuffer[Int]

    def createRandomList(count: Int) {
      if (count > 0) {
        (rnd.nextInt(1000) - 500) +=: xs
        createRandomList(count - 1)
      }
    }

    createRandomList(500)
    xs.toList    
  }
  
  def randomSet3 = (randomSet1, 10000)
  
  def intervalSet1 = (
      List(Interval(2, 3), Interval(4, 5), Interval(-1, 0), Interval(-5, -3)),
      List(Interval(-5, -3), Interval(-1, 0), Interval(2, 3), Interval(4, 5))
      )
      
  def intervalSet2 = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs  = new scala.collection.mutable.ListBuffer[Interval[Int]]
    
    def createRandomIntervals(count: Int) {
      val x = rnd.nextInt(1000) - 500
      val y = rnd.nextInt(200)
      Interval(x, y) +=: xs
    }
    
    createRandomIntervals(300)
    xs.toList
  }

  def verifySortedIntervals(sortedIntervals: List[Interval[Int]]): Boolean = {
    def checkOrdering(xs: List[Interval[Int]], verifyResult: Boolean) : Boolean = xs match {
      case Nil => verifyResult
      case head :: Nil => verifyResult
      case head :: ys =>
        verifyResult && checkOrdering(ys, head.start < ys.head.start || (head overlap ys.head))
    }
    
    checkOrdering(sortedIntervals, true)
  }

  
  val comparisonSortSets = List(
      ("Insertion Sort", Sorter.insertionSort[Int] _, Sorter.insertionSort0[Int] _),
      ("Selection Sort", Sorter.selectionSort[Int] _, Sorter.selectionSort0[Int] _),
      ("Merge Sort", Sorter.mergeSort[Int] _, Sorter.mergeSort0[Int] _),
      ("Heap Sort", Sorter.heapSort[Int] _, Sorter.insertionSort0[Int] _),
      ("Quick Sort", Sorter.quickSort[Int] _, Sorter.mergeSort0[Int] _)
  )

  val linearTimeSortSets = List(
      ("Counting Sort", Sorter.countingSort _, Sorter.quickSort[Int]_),
      ("Radix Sort", Sorter.radixSort _, Sorter.quickSort[Int] _),
      ("Bucket Sort", Sorter.bucketSort _, Sorter.quickSort[Int] _)
  )
      
  comparisonSortSets foreach { sortSet =>
    val rndSet1 = randomSet1
    val rndSet2 = randomSet2
      
    sortSet._1 should behave like comparisonSort(sortSet._2) (
      (testSet1, sortSet._3(testSet1)),
      (testSet2, sortSet._3(testSet2)),
      (rndSet1, sortSet._3(rndSet1)),
      (rndSet2, sortSet._3(rndSet2))
    )
  }
  
  linearTimeSortSets foreach { sortSet =>
    val (rndSet, maxNum) = randomSet3
    
    sortSet._1 should behave like linearTimeSort(sortSet._2) (
      (testSet2, 10, sortSet._3(testSet2)),
      (rndSet, maxNum, sortSet._3(rndSet))
    )
  }
  
  "Fuzzy Sort" should "perform correctly" in {
    (Sorter.fuzzySort(intervalSet1._1)) should equal (intervalSet1._2)    
  }
  
  it should "pass the verification" in {
    val sortResult = Sorter.fuzzySort(intervalSet2)
    (verifySortedIntervals(sortResult)) should be (true)
  }
}