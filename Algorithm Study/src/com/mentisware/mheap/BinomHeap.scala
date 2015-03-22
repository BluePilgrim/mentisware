package com.mentisware.mheap

import Ordering.Implicits._


//
// This time, more functional programming style has been applied.
// Since a full persistent binomial heap incurs O(n)-time operations for update/delete,
// I used "var" types for elem. Instead, all other fields are in "val" type.
//

class BinomHeap[T: Ordering](src: Seq[Element[T]] = Nil, b: Boolean = true) extends MergeableHeap[T] {
  private var binomTrees : List[BinomTree[T]] = Nil
  private var minTree: BinomTree[T] = _
  private val refMap = scala.collection.mutable.Map[Element[T], BinomTree[T]]()
  
  val isMinHeap = b
  private val lessThan = if (b) { (x: T, y: T) => x < y } else { (x: T, y: T) => x > y}
  
  def isEmpty = minTree == null
  def getSize() = (0 /: binomTrees)((s: Int, t: BinomTree[T]) => s + t.size)
  
  def insert(e: Element[T]) {
    // assume the binomTrees list is already constructed in tree-order increasing order
    def addBinomTree(
        n: BinomTree[T],
        treeList: List[BinomTree[T]]) : List[BinomTree[T]] = {
      if (treeList == Nil || n.order < treeList.head.order) {
        if (minTree == null || lessThan(e, minTree.elem)) minTree = n
        n :: treeList
      } else {
        assert(n.order == treeList.head.order)
        val mergedBinomTree =
          if (lessThan(n.elem, treeList.head.elem)) HighOrderTree(n, treeList.head)
          else HighOrderTree(treeList.head, n)
        addBinomTree(mergedBinomTree, treeList.tail)
      }
    }
    
    val n = ZeroOrderTree(e)
    binomTrees = addBinomTree(n, binomTrees)
    refMap += (e -> n)
    
    // augment minTree with high order node
    minTree = minTree.highOrderTree
  }
  
  def head = {
    if (minTree == null) None else Option(minTree.elem)
    // only consider the roots of the list
//    if (isEmpty) None
//    else {
//      val minimum =
//        (binomTrees.head.elem /: binomTrees.tail)(
//          (e: Element[T], t: BinomTree[T]) => if (lessThan(t.elem, e)) t.elem else e)
//      Option(minimum)
//    }
  }
    
  def extractHead() = {
    if (isEmpty) None
    else {
      val res = Option(minTree.elem)
      refMap.remove(minTree.elem)
      removeOneRoot(minTree)
      res
    }
  }
  
  def union(that: this.type) {
    def merge(
        x: List[BinomTree[T]], y: List[BinomTree[T]], c: BinomTree[T]
        ): List[BinomTree[T]] = {
      if (x == Nil) {
        if (y == Nil) if (c != null) List(c) else Nil
        else if (c != null)
          if (c.order == y.head.order) {
            val carry =
              if (lessThan(c.elem, y.head.elem)) HighOrderTree(c, y.head)
              else HighOrderTree(y.head, c)
            merge(Nil, y.tail, carry)
          } else {
            assert(c.order < y.head.order)
            c :: y
          }
        else y
      } else if (y == Nil) {
        if (c != null)
          if (c.order == x.head.order) {
            val carry =
              if (lessThan(c.elem, x.head.elem)) HighOrderTree(c, x.head)
              else HighOrderTree(x.head, c)
            merge(x.tail, Nil, carry)
          } else {
            assert(c.order < x.head.order)
            c :: x
          }
        else x
      } else {
        if (c != null) {
          assert(c.order <= x.head.order && c.order <= y.head.order)
          if (c.order == x.head.order) {
            val carry =
              if (lessThan(c.elem, x.head.elem)) HighOrderTree(c, x.head)
              else HighOrderTree(x.head, c)
            if (c.order == y.head.order) y.head :: merge(x.tail, y.tail, carry)
            else merge(x.tail, y, carry)
          } else if (c.order == y.head.order) {
            val carry =
              if (lessThan(c.elem, y.head.elem)) HighOrderTree(c, y.head)
              else HighOrderTree(y.head, c)
            merge(x, y.tail, carry)
          } else c :: merge(x, y, null)
        } else if (x.head.order == y.head.order)
          merge(x.tail, y.tail,
              if (lessThan(x.head.elem, y.head.elem)) HighOrderTree(x.head, y.head)
              else HighOrderTree(y.head, x.head)
              )
        else if (x.head.order < y.head.order) x.head :: merge(x.tail, y, null)
        else y.head :: merge(x, y.tail, null)
      }
    }
    
    binomTrees = merge(this.binomTrees, that.binomTrees, null)

    // set new minTree
    minTree =
      if (binomTrees != Nil) (binomTrees.head /: binomTrees.tail)(
          (x, y) => if (lessThan(x.elem, y.elem)) x else y)
      else null
      
    // merge refMap
    refMap ++= that.refMap
  }

  def updateKey(e: Element[T], k: T) {    // decrease key for min heap, increase key for max heap
    def promote(t: BinomTree[T]): BinomTree[T] = {
      val p = t.parent
      if (p != null && lessThan(t.elem, p.elem)) {
        t.updateElem(p.updateElem(t.elem))
        refMap += (t.elem -> t)
        promote(p)
      } else t
    }

    if (lessThan(k, e)) {
      refMap.remove(e) match {
        case Some(n) =>
          e.updateKey(k)
          val t = promote(n)
          refMap += (e -> t)

          if (lessThan(e, minTree.elem)) minTree = t.highOrderTree
        case _ => error("ref map is corrupted for " + e)
      }
    }
  }
  
  def delete(e: Element[T]) {
    def promoteToRoot(t: BinomTree[T]): BinomTree[T] = {
      val p = t.parent
      if (p != null) {
        t.updateElem(p.updateElem(t.elem))
        refMap += (t.elem -> t)
        promoteToRoot(p)
      } else t
    }

    refMap.remove(e) match {
      case Some(n) =>
        val r = promoteToRoot(n).highOrderTree    // augmentation is necessary
        assert(r.elem == e)
        removeOneRoot(r)
      case _ => error("ref map is corrupted for " + e)
    }
  }
  
  def validate() {
    def checkMinHeapProperty(n: BinomTree[T]) {
      n match {
        case x: ZeroOrderTree[T] =>    // return normal
        case y: HighOrderTree[T] =>
          if (lessThan(y.leftMostChild.elem, y.elem))
            error("Minheap property is broken: parent = " + y.elem + " child = " + y.leftMostChild.elem)
          checkMinHeapProperty(y.base)
          checkMinHeapProperty(y.leftMostChild)
      }
    }

    if (binomTrees != Nil) {
      // check if the list is ordered correctly according to the tree order
      var prevOrder = binomTrees.head.order
      binomTrees.tail.foreach { x =>
        if (x.order <= prevOrder)
          error("Binomial Tree List Order is broken: prev Order = " + prevOrder + " current = " + x.order)
        prevOrder = x.order
      }
      
      // check the min heap property & minTree value's correctness
      binomTrees.foreach { x =>
        if (lessThan(x.elem, minTree.elem))
          error("minTree value is corrupted: minTree = " + minTree + " current = " + x)
        checkMinHeapProperty(x)
      }
      
      // check the ref map is valid
      refMap foreach { case (elem, node) =>
        if (elem != node.elem)
          error("corrupted ref map for element = " + elem)
      }
    }
  }

  private def removeOneRoot(r: BinomTree[T]) {
    def mergeChildIntoList(tree: BinomTree[T]): (
        List[BinomTree[T]], List[BinomTree[T]], BinomTree[T]) = tree match {
      case b: ZeroOrderTree[T] =>
        // it has no child, so just return the list
        // the elem is just removed from the set
        (binomTrees, Nil, null)
      case x: HighOrderTree[T] =>
        val (treesToConsider, treeList, carry) = mergeChildIntoList(x.base)
        val h = treesToConsider.head
        
        // remove the connection to the root
        x.leftMostChild.p = null
//        x.base.p = null
        
        if (x.leftMostChild.order == h.order) {
          val c =
            if (lessThan(x.leftMostChild.elem, h.elem)) HighOrderTree(x.leftMostChild, h)
            else HighOrderTree(h, x.leftMostChild)
            
          if (carry != null) {      // put carry into the current order and pass up new carry
            (treesToConsider.tail, carry :: treeList, c)
          } else (treesToConsider.tail, treeList, c)
        } else {
          assert(x.leftMostChild.order < h.order)
          if (carry != null) {
            val c = 
              if (lessThan(x.leftMostChild.elem, carry.elem)) HighOrderTree(x.leftMostChild, carry)
              else HighOrderTree(carry, x.leftMostChild)
            (treesToConsider, treeList, c)
          } else (treesToConsider, x.leftMostChild :: treeList, null)
        }
    }

    r match {
      case x : HighOrderTree[T] =>
        // when x's order is k, then there come k subtrees emerge whose orders are (0 to k-1)
        val (treesToConsider, treeList, carry) = mergeChildIntoList(x)
        assert(minTree != r || treesToConsider.head == minTree)
        val remainingTrees = if (treesToConsider != Nil) treesToConsider.tail else Nil
        binomTrees =
          if (carry != null) treeList.reverse ::: (carry :: remainingTrees)
          else treeList.reverse ::: remainingTrees
        
      case b : ZeroOrderTree[T] =>
        // just remove the tree from the list
        assert(b == binomTrees.head)
        binomTrees = binomTrees.tail
    }
    
    // set new minTree
    minTree =
      if (binomTrees != Nil)
        (binomTrees.head /: binomTrees.tail)((x, y) => if (lessThan(x.elem, y.elem)) x else y)
      else null

//    println("remove a root: r = " + r.elem + " new minTree = " +
//        (if (minTree == null) null else minTree.elem))
  }

  // insert elements in src
  src foreach (insert _)

}

private sealed abstract class BinomTree[T: Ordering] {
  def order: Int
  def elem: Element[T]
  def updateElem(n: Element[T]): Element[T]
  def size = 1 << order

  var p: HighOrderTree[T] = null
  def parent: BinomTree[T] = {
    if (p == null) null
    else if (p.leftMostChild == this) p
    else p.parent
  }

  // From "parent" to "t" lie the leftmost children
  // and from "t" to ZeroOrder Tree "t.base" lie the rightmost children
  def leftChildren: List[BinomTree[T]] = {
    if (p == null || p.leftMostChild == this) Nil
    else
      this match {
        case x: HighOrderTree[T] => x.leftMostChild :: p.leftChildren
        case _ => p.leftChildren
      }
  }

  // the direct child can be retrieved via leftChildren
  def rightChildren: List[BinomTree[T]] = this match {
    case x: ZeroOrderTree[T] => Nil
    case y: HighOrderTree[T] =>
      y.base match {
        case a: ZeroOrderTree[T] => Nil
        case b: HighOrderTree[T] => b.leftMostChild :: b.rightChildren
      }
  }

  def children: List[BinomTree[T]] = {
    // rightmost children comes first
    leftChildren ::: rightChildren.reverse
  }
  
  def highOrderTree: BinomTree[T] = {
    if (p == null || p.base != this) this
    else p.highOrderTree
  }
}

// the semantics of "p" of base and leftMostChild are different from each other.
// leftMostChild's p is the real parent in the binomial tree
// base's p is a medium to get a reference to nesting tree structure which enables retrieving children.

private case class ZeroOrderTree[T: Ordering](var elem: Element[T]) extends BinomTree[T] {
  def order = 0
  def updateElem(n: Element[T]): Element[T] = { val org = elem; elem = n; org }
}

private case class HighOrderTree[T: Ordering](
    base: BinomTree[T], leftMostChild: BinomTree[T]) extends BinomTree[T] {
  require(base.order == leftMostChild.order)
  def elem = base.elem
  def updateElem(n: Element[T]) = base.updateElem(n)
  def order = base.order + 1
  
  base.p = this
  leftMostChild.p = this
}

object BinomHeap {
  def apply[T: Ordering](xs: Seq[Element[T]] = Nil, b: Boolean = true) = new BinomHeap[T](xs, b)
}
