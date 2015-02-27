package com.mentisware.mheap

import scala.collection.mutable.Map
import scala.math._
import Ordering.Implicits._
  


class FibHeap[T: Ordering](src: Seq[Element[T]] = Nil, b: Boolean = true) extends MergeableHeap[T] {
  private var rootHead: FibTree[T] = null
  private val refMap = Map[Element[T], FibTree[T]]()
  private var size = 0          // # of node in the heap
    
  val isMinHeap = b                       // true for min heap, false for max heap
  def isEmpty = rootHead == null
  def head: Option[Element[T]] = if (isEmpty) None else Some(rootHead.elem)
  def getSize() = size

  val lessThan = if (b) { (x: T, y: T) => x < y } else { (x: T, y: T) => x > y}

  def insert(e: Element[T]) {
    val n = FibTree(e)
    if (isEmpty) rootHead = n
    else {
      rootHead.prepend(n)
      if (lessThan(e, rootHead.elem)) rootHead = n
    }
    size += 1
    
    refMap += (e -> n)
  }

  def union(that: this.type) {
    if (isMinHeap != that.isMinHeap) error("different type of mergeable heap")
    
    if (isEmpty) rootHead = that.rootHead
    else {
      // concatenate the root lists
      rootHead.concatenate(that.rootHead)
      if (!that.isEmpty && lessThan(that.rootHead.elem, rootHead.elem)) rootHead = that.rootHead
    }

    size += that.size
    that.rootHead = null        // destroy that
    
    refMap ++= that.refMap      // I don't like this part. It may incur O(n) time overhead.
  }

  def extractHead(): Option[Element[T]] = {
    if (!isEmpty) {
      val res = rootHead.elem
      
      // promote each child of the head to the root list by concatenating the children to head's next sibling
      promoteToRoot(rootHead.childHead)

      val l = rootHead.remove()    // remove rootHead from the root list
      rootHead = if (l == null) null else consolidate(l)
      size -= 1

//      println("extract " + res)
      refMap.remove(res)
      Some(res)
    } else None
  }
  
  // Since it is a Fibonacci Heap, the max degree of any node is <= log phi (n)
  private def maxDegree = (log(size.toDouble) / log(1.618)).toInt
  
  private def promoteToRoot(l: FibTree[T]) {
    if (l != null) {
      var n = l
      do {
        n.parent = null
        n = n.next
      } while (n != l)
      
      rootHead.concatenate(l)
    }
  }
    
  private def consolidate(rl: FibTree[T]): FibTree[T] = {
    def link(c: FibTree[T], p: FibTree[T]) = {
      // remove c from the root list and make c a child of p
      assert(c.parent == null && p.parent == null)
      val l = c.remove()
      p.addChild(c)
      c.mark = false
      p
    }

    val d = maxDegree
    val fibTrees = Array.fill[FibTree[T]](d+1)(null)  // fibTrees will maintain roots per degree
    
    // for each node in the root list, perform consolidation with updating fibTrees
    // During the consolidation, pointers of a root might be changed.
    // So the initial snap shot of the root list should be maintained.
    var n = rl
    val rootList = new scala.collection.mutable.ListBuffer[FibTree[T]]()
    do {
      rootList += n
      n = n.next
    } while (n != rl)

    rootList foreach { w =>
      var cTree = w
      var cDegree = w.degree

//      println("consolidate for node " + cTree.elem)
      while (cDegree <= d+1 && fibTrees(cDegree) != null) {
        // compare the key and choose the parent and the child
        val y = fibTrees(cDegree)
        cTree = if (lessThan(y.elem, cTree.elem)) link(cTree, y) else link(y, cTree)
        assert(cTree.parent == null)
        fibTrees(cDegree) = null      // now a fibTree with cDegree is removed
        cDegree += 1
      }
      
      fibTrees(cDegree) = cTree
    }
    
    // construct a new root list from fibTrees and choose the head
    // The roots in fibTrees are already connecting each other correctly via proper reference updates.
    // So, finding the minimum root will suffice.
    var h: FibTree[T] = null

    fibTrees foreach { r =>
      if (r != null)
        if (h == null || lessThan(r.elem, h.elem)) h = r
    }
/*
    (h /: fibTrees) { (list: FibTree[T], n: FibTree[T]) =>
      if (n != null) {
        assert(n.parent == null)
        n.prepend(list)
        if (h == null || lessThan(n.elem, h.elem)) h = n
        n
      } else list
    }
*/    
    assert(h != null)
    h
  }
  
  def updateKey(e: Element[T], k: T) {
    if (lessThan(k, e)) {
      refMap.remove(e) match {
        case Some(n) =>
          e.updateKey(k)
          refMap += (e -> n)
          
          val p = n.parent
          if (p != null && lessThan(k, p.elem)) {
            cut(n, p)
            cascadingCut(p)
          }
          
          if (lessThan(k, rootHead.elem)) rootHead = n
        case _ => error("ref map is corrupted for " + e)
      }
    }
  }
  
  def delete(e: Element[T]) {
    // make the element as a new root and then extract it
    refMap.remove(e) match {
      case Some(n) =>
        val p = n.parent
        if (p != null) {
          cut(n, p)
          cascadingCut(p)
        }

        // promote each child of n to root list by concatenating the children to head's next sibling
        promoteToRoot(n.childHead)
  
        val l = n.remove()    // remove n from the root list
        rootHead = if (l == null) null else consolidate(l)
        size -= 1

      case _ => // error("ref map is corrupted for " + e)
    }
  }

  private def cut(c: FibTree[T], p: FibTree[T]) {
    // remove c from p's child list and promote to root
    p.removeChild(c)
    rootHead.prepend(c)
    c.mark = false
  }
  
  private def cascadingCut(n: FibTree[T]) {
    val p = n.parent
    if (p != null) {
      if (!n.mark) n.mark = true
      else {
        cut(n, p)
        cascadingCut(p)
      }
    }
  }
  
  
  def validate() {
    def checkMinHeapProperty(p: FibTree[T]) {
      if (p.degree > 0) {
        var n = p.childHead
        assert(n != null)
        
        do {
          if (lessThan(n.elem, p.elem))
            error("min heap property broken: parent = " + p.elem + " node = " + n.elem)
          checkMinHeapProperty(n)
          n = n.next
        } while (n != p.childHead)
      }
    }
    
    if (!isEmpty) {
      var r = rootHead
      do {
        // check if a root's parent is set to null
        if (r.parent != null) error("root's parent is not null for node = " + r.elem)
        
        // check if the rootHead is the minimum in the root list
        if (lessThan(r.elem, rootHead.elem))
          error("incorrect root head: root head = " + rootHead.elem + " root = " + r.elem)
        // check if its descendants are not less than the root
        checkMinHeapProperty(r)
        
        r = r.next
      } while (r != rootHead)
        
      // check the ref map is valid
      refMap foreach { case (elem, node) =>
        if (elem != node.elem)
          error("corrupted ref map for element = " + elem)
      }
    }
  }
  
  // insert elements in src
  src foreach (insert _)
}

object FibHeap {
  def apply[T: Ordering](xs: Seq[Element[T]] = Nil, b: Boolean = true) = new FibHeap[T](xs, b)
}


object FibTree {
  def apply[T: Ordering](e: Element[T]) = new FibTree[T](e)
}

class FibTree[T: Ordering](e: Element[T]) {
  val elem = e      // elem's key might be changed.
  var prev = this    // doubly-linked list
  var next = this
  var parent: FibTree[T] = _
  var childHead: FibTree[T] = _
  
  var degree = 0    // num of children
  var mark = false  // indicate whether the node x has lost a child since the last time x was made the child of other node

  override def toString = elem + "(" + prev.elem + ", " + next.elem + ")"
  
  def prepend(n: FibTree[T]) {
    if (n != null) {
      if (prev != this) {
        val p = prev
        p.next = n
        n.prev = p
        this.prev = n
        n.next = this
      } else {
        assert(next == this)
        this.prev = n
        this.next = n
        n.prev = this
        n.next = this
      }
    
//    n.parent = this.parent      // set-parent operation had better be done explicitly.
    }
  }
  
  def remove() = {  // remove this from the list and return the next node
    if (next != this) {
      val p = this.prev
      val n = this.next
      p.next = n
      n.prev = p
      n
    } else null
  }
  
  def addChild(c: FibTree[T]) {
    if (c != null) {
      if (childHead == null) {
        childHead = c
        c.next = c
        c.prev = c
      } else childHead.prepend(c)
      
      c.parent = this
      degree += 1
    }
  }
  
  def removeChild(c: FibTree[T]) {
    if (c != null) {
      val l = c.remove()
      c.parent = null
      degree -= 1
      if (childHead == c) childHead = l
    }
  }
  
  def concatenate(that: FibTree[T]) {      // concatenate two lists
    if (that != null) {
      // that comes next to the last element of this list
      // this comes next to the last element of that list
      var thisLast = this.prev
      var thatLast = that.prev
      
      thisLast.next = that
      that.prev = thisLast
      
      thatLast.next = this
      this.prev = thatLast
    }
  }
}