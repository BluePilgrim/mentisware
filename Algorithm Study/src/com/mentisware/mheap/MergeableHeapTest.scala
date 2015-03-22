package com.mentisware.mheap

import com.mentisware.test.UnitSpec
import com.mentisware.sort.Sorter

// don't use a case class with a var. It's malfunctioning with a map.
class IntElem(var key: Int) extends Element[Int] {
  def updateKey(n: Int): Int = { val org = key; key = n; org }
}

trait MergeableHeapBehavior { this: UnitSpec =>
  val sort = Sorter.quickSort[Int] _
  
  def setupCorrectly(xs: List[Int])(createHeap: (Seq[Element[Int]], Boolean)=> MergeableHeap[Int]) {
    val es = xs.map(new IntElem(_))
    val minHeap = createHeap(Nil, true)
    val maxHeap = createHeap(Nil, false)
    
    it should "return the same min heap from empty and inital list" in {
      val heap = createHeap(es, true)
      
      es foreach { minHeap.insert }
      
      minHeap.validate()
      heap.validate()
      
      (minHeap.getSize()) should equal (heap.getSize())
      (minHeap.head.get.key) should equal (heap.head.get.key)
    }
    
    it should "return the same max heap from empty and inital list" in {
      val heap = createHeap(es, false)
      
      es foreach { maxHeap.insert }
      
      maxHeap.validate()
      heap.validate()
      
      (maxHeap.getSize()) should equal (heap.getSize())
      (maxHeap.head.get.key) should equal (heap.head.get.key)
    }
    
    it should "be empty after n extract operations" in {
      for (i <- 0 until es.size) {
        (minHeap.extractHead()) should not equal (None)
        minHeap.validate()
        (maxHeap.extractHead()) should not equal (None)
        maxHeap.validate()
      }
      
      (minHeap.getSize()) should equal (0)
      (maxHeap.getSize()) should equal (0)
    }
    
    it should "return None from head in empty heap" in {
      (minHeap.head) should equal (None)
      (maxHeap.head) should equal (None)
    }
    
    it should "do n inserts and n deletes to be empty" in {
      for (i <- 0 until es.size by 2) {
        minHeap.insert(es(i))
        maxHeap.insert(es(i))
      }
      
      for (i <- 0 until es.size by 2) {
        minHeap.delete(es(i))
        maxHeap.delete(es(i))
      }
      
      (minHeap.getSize()) should equal (0)
      (maxHeap.getSize()) should equal (0)      
    }
  }

  def performMHeapOperations(xs: List[Int])(createHeap: (Seq[Element[Int]], Boolean)=> MergeableHeap[Int]) {
    val es = xs.map(new IntElem(_))
    val ys = sort(xs).toVector
    val zs = ys.reverse

    it should "return min value correctly" in {
      val minHeap = createHeap(es, true)

      for (i <- 0 until es.size) {
        (minHeap.extractHead().getOrElse(new IntElem(0)).key) should equal (ys(i))
      }
    }
    
    it should "return max value correctly" in {
      val maxHeap = createHeap(es, false)

      for (i <- 0 until es.size) {
        (maxHeap.extractHead().getOrElse(new IntElem(0)).key) should equal (zs(i))
      }
    }

    it should "do union operation for min heap correctly" in {
      val minHeap1 = createHeap(es, true)
      val es2 = zs.slice(0, zs.size/2).map { x => new IntElem(x + 4) }
      val minHeap2 = createHeap(es2, true)
      val s1 = minHeap1.getSize()
      val s2 = minHeap2.getSize()
      
      minHeap1 union minHeap2.asInstanceOf[minHeap1.type]
      
      (minHeap1.getSize()) should equal (s1 + s2)
      (minHeap1.head.get.key) should equal (sort(xs ++ zs.slice(0, zs.size/2).map(_+4)).head)
    }

    it should "do union operation for max heap correctly" in {
      val maxHeap1 = createHeap(es, false)
      val es2 = zs.slice(zs.size/2, zs.size).map { x => new IntElem(x - 4) }
      val maxHeap2 = createHeap(es2, false)
      val s1 = maxHeap1.getSize()
      val s2 = maxHeap2.getSize()
      
      maxHeap1 union maxHeap2.asInstanceOf[maxHeap1.type]
    }

    // since es can be modified via updateKey operations, these test cases comes at the last.
    it should "update key operations correctly for min heap" in {
      val minHeap = createHeap(es, true)
      val min = minHeap.head.get
      val newKey = min.key - 1
      
      minHeap.updateKey(es(0), newKey)
      (minHeap.head.get.key) should equal (newKey)
      
      minHeap.updateKey(es(0), newKey + 2)
      (minHeap.head.get.key) should equal (newKey)

      for (i <- 1 until es.size by 2) minHeap.updateKey(es(i), es(i).key - i)
      
      minHeap.validate()
    }

    it should "update key operations correctly for max heap" in {
      val maxHeap = createHeap(es, false)
      val max = maxHeap.head.get
      val newKey = max.key + 1
      
      maxHeap.updateKey(es(0), newKey)
      (maxHeap.head.get.key) should equal (newKey)
      
      maxHeap.updateKey(es(0), newKey - 2)
      (maxHeap.head.get.key) should equal (newKey)

      for (i <- 1 until es.size by 2) maxHeap.updateKey(es(i), es(i).key + i)

      maxHeap.validate()
    }
  }
}