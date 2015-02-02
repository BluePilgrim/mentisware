package com.mentisware.sort

class Heap[T : Ordering](data: List[T], isMaxHeap: Boolean = true) {
  import scala.collection.mutable.ArrayBuffer
  import Ordering.Implicits._

  private val arr = data.to[ArrayBuffer]
  private var size = arr.length
  private val compare = if (isMaxHeap) { (x: T, y: T) => x > y } else { (x: T, y: T) => x < y }
  
  private def leftChild(i: Int) =  (i << 1) + 1
  private def rightChild(i: Int) = (i + 1) << 1
  private def parent(i: Int) = (i - 1) >> 1
  
  private def heapify(i: Int) {
    val l = leftChild(i)
    val r = rightChild(i)
    var choice = i
    val org = arr(i)
    
    /* determine the new key for i */
    if (l < size && compare(arr(l), org)) choice = l
    if (r < size && compare(arr(r), arr(choice))) choice = r
    if (choice != i) {
      arr(i) = arr(choice)  // swap keys
      arr(choice) = org
      heapify(choice)
    }
  }

  private def updateWithParent(i: Int) {
    if (i > 0) {
      val p = parent(i)
      val elem = arr(i)
  
      if (compare(elem, arr(p))) {
        arr(i) = arr(p)
        arr(p) = elem
        updateWithParent(p)
      }
    }
  }


  private def rebuild(i: Int) {
    if (i < size/2) {
      rebuild(i+1)
      heapify(i)
    }
  }
  
  def getHead(): Option[T] = if (size == 0) None else Some(arr(0))

  def extractHead(): Option[T] = {
    if (size == 0) None
    else {
      val head = Some(arr(0))
      size -= 1
      arr(0) = arr(size)
      heapify(0)
      head
    }
  }

  def add(x: T) {
    arr += x      // wrong!!!!
    arr(size) = x
    updateWithParent(size)
    size += 1
  }

  def update(i: Int, x: T) {
    if (i >= 0 && i < size) {
      if (compare(x,arr(i))) {         // the node might have to move upward
        arr(i) = x
        updateWithParent(i)
      } else if (compare(arr(i), x)) {  // the node might have to move downward
        arr(i) = x
        heapify(i)
      }
    }
  }
  
  def getContents = arr.toList.slice(0, size)
  def getLength = size
  
  rebuild(0)    // build a heap from the given list
}