package com.mentisware.mheap

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import Ordering.Implicits._

class BinHeap[T: Ordering](src: Seq[Element[T]] = Nil, b: Boolean = true) extends MergeableHeap[T] {
  private val heapData: ArrayBuffer[Element[T]] = src.to[ArrayBuffer]
  private val indexMap = Map[Element[T], Int]()
  private var size = heapData.length
//  type MHeap[T] = BinHeap[T]
  
  def isEmpty = size == 0
  def getSize() = size
  
  val compare =
    if (b) { (x: Element[T], y: Element[T]) => x.key <= y.key }
    else { (x: Element[T], y: Element[T]) => x.key >= y.key }
  val isMinHeap = b
  
  def insert(e: Element[T]) {
    if (heapData.length > size)
      heapData(size) = e
    else
      heapData += e
    indexMap.update(heapData(size), size)
    
    updateWithParent(size)
    size += 1
  }
  
  def head = if (size > 0) Some(heapData(0)) else None
  
  def extractHead() = {
    if (size > 0) {
      val head = heapData(0)
      size -= 1
      indexMap.remove(heapData(0))
      heapData(0) = heapData(size)
      indexMap.update(heapData(0), 0)
      heapify(0)
      Some(head)
    } else None
  }
  
  def union(that: this.type) {
    heapData.trimEnd(heapData.length - size)    // strip off meaningless data
    heapData.appendAll(that.heapData.view(0, that.size))
    size += that.size
    rebuild()
  }

  // decrease for min heap and increase for max heap
  def updateKey(e: Element[T], k: T) {
    if (isMinHeap && k < e.key || !isMinHeap && k > e.key) {
      indexMap.remove(e) match {
        case Some(i) =>
          e.updateKey(k)
          indexMap.update(e, i)
          updateWithParent(i)
        case _ => error("index map is corrupted for " + e)
      }
    }
  }
  
  // for delete, swap the element with the last one and heapify that one.
  def delete(e: Element[T]) {
    indexMap.remove(e) match {
      case Some(i) =>
        size -= 1
        heapData(i) = heapData(size)
        indexMap.update(heapData(i), i)
        heapify(i)
      case _ => error("index map is corrupted for " + e)
    }
  }
  
  def validate() {
    // check the integrity of index map
    for (i <- 0 until size)
      if (i != indexMap(heapData(i)))
        error("corrupted index map for index = " + i + " : " + indexMap(heapData(i)))
      
    // check the mergeable heap property
    for (i <- size-1 until 0 by -1 )
      if (!compare(heapData(parent(i)), heapData(i)))
        error("mergeable heap property broken at i = " + i + "("+ heapData(i) + "), p = " +
            parent(i) + "(" + heapData(parent(i)))
  }
  
  
  private def leftChild(i: Int) =  (i << 1) + 1
  private def rightChild(i: Int) = (i + 1) << 1
  private def parent(i: Int) = (i - 1) >> 1

  // move down i th key into the appropriate position
  private def heapify(i: Int) {
    val l = leftChild(i)
    val r = rightChild(i)
    val e = heapData(i)

    // find the smallest key among the current, left child, and right child
    // and then swap the position
    val s =
      if (l < size) {
        if (r < size) {
          if (compare(heapData(l), heapData(r)))
            if (compare(heapData(l), heapData(i))) l else i
          else
            if (compare(heapData(r), heapData(i))) r else i
        } else if (compare(heapData(l), heapData(i))) l else i
      } else i    // both l and r are out of bounds      
      
    if (i != s) {
      heapData(i) = heapData(s)
      heapData(s) = e
      
      // update the index map
      indexMap.update(heapData(i), i)
      indexMap.update(heapData(s), s)
      
      heapify(s)
    }
  }

  // move up to the adequate position
  private def updateWithParent(i: Int) {
    if (i > 0) {
      val p = parent(i)
      val e = heapData(i)
  
      if (compare(e, heapData(p))) {
        heapData(i) = heapData(p)
        heapData(p) = e
        
        indexMap.update(heapData(i), i)
        indexMap.update(heapData(p), p)
        
        updateWithParent(p)
      }
    }
  }

  // rebuild the whole data : heapify internal nodes (0 until (n/2))
  private def rebuild() {
    def _rebuild(i : Int) {
      if (i >= 0) {
        heapify(i)
        _rebuild(i - 1)
      }
    }
    
    // clear the index map and set the latter half with the current index
    indexMap.clear()
    for (i <- 0 until size) indexMap.update(heapData(i), i)
    
    _rebuild(size / 2 - 1)
  }

  rebuild()
}

object BinHeap {
  def apply[T: Ordering](xs: Seq[Element[T]] = Nil, b: Boolean = true) = new BinHeap[T](xs, b)
}