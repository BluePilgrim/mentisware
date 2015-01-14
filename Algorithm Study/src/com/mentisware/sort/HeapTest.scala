package com.mentisware.sort
import com.mentisware.test.UnitSpec

class HeapTest extends UnitSpec{
  import scala.collection.mutable.ArrayBuffer
  
  def nullData: List[Int] = Nil
  def testSet1 = (
      List(4, 1, 3, 2, 16, 9, 10, 14, 8, 7),
      List(16, 14, 10, 8, 7, 9, 3, 2, 4, 1),
      Some(16)
  )

  def testSet2: List[Int] = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs = new scala.collection.mutable.ListBuffer[Int]

    def createRandomList(count: Int) {
      (rnd.nextInt(500) -250) +=: xs
    }

    createRandomList(300)
    xs.toList
  }
  
  val emptyHeap = new Heap[Int](Nil)
  "An empty heap" should "return None for getHead and extract requests" in {
    (emptyHeap.getHead) should equal (None)
    (emptyHeap.extractHead) should equal (None)
  }

  val (xs, ys, max) = testSet1
  val heap = new Heap[Int](xs)

  "A heap" should "be created from a unsorted list" in {
    (heap.getContents) should equal (ys) 
  }
  
  it should "be able to extract max reducing the size by one" in {
    val len = heap.getLength
    (heap.extractHead) should equal (max)
    (heap.getLength) should equal (len - 1)
  }

  
  "A blank heap" should "constructed from add with same result as a new heap with the list" in {
    val xs = testSet2
    val heap1 = new Heap[Int](xs)
    val heap2 = new Heap[Int](Nil)
    
    xs foreach heap2.add
    
    (heap1.getContents) should equal (heap2.getContents)
  }
}