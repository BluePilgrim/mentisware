package com.mentisware.tree.btree

import Ordering.Implicits._

import com.mentisware.tree.Element
import com.mentisware.tree.SearchTree


// immutable B Tree dynamic set
// leaf nodes may have twice more data than internal nodes by utilizing the child pointer space.

sealed abstract class BTree[T: Ordering] extends SearchTree[T] {
  def t: Int    // B Tree degree

  val elems: Vector[Element[T]]
  def keys = elems.map(_.key)
  override def numOfKeys = elems.length
  def children: Vector[BTree[T]]
  
  def isLeaf: Boolean
  def isRoot: Boolean
  def isEmpty = false

  // the keys are stored in increasing order, so we may use binary search or linear search
  // i th child contains keys <= i th key
  // it returns the index "i", key(i) >= k or i == keys.length
  // when found, "i" is the key index.
  // when not found, key(i) > k && (key(i+1) < k || i == keys.length)
  protected[btree] def findIndex(k: T): (Int, Boolean) = {
    var i = 0

    // if the minimum degree is low, use linear search, otherwise, use binary search
    if (t < 8) {
      while (i < numOfKeys && k > elems(i).key ) i += 1
      
      (i, i < numOfKeys && k == elems(i).key )
    } else {
      // if found, just return i
      // otherwise, if key(i) < k, then i + 1, if not, then i
      if (numOfKeys == 0) (0, false)
      else {
        var s = 0
        var e = numOfKeys - 1
        
        do {
          i = (s + e) / 2
          if (k > elems(i).key) s = i + 1
          else if (k < elems(i).key) e = i - 1
          else s = e + 2        
        } while (s <= e)
        
        if (s == e+2) (i, true)
        else if (k < elems(i).key) (i, false)
        else (i+1, false)
      }
    }
  }
    
  def apply(k: T) = {
    val (idx, found) = findIndex(k)
    
    if (found) elems(idx)
    else if (!isLeaf) children(idx).apply(k)
    else error(k + "not in the tree")
  }
  
  def maximum = {
    if (isEmpty) None
    else if (isLeaf) Some(elems(numOfKeys-1))
    else children(numOfKeys).maximum
  }
  
  def minimum = {
    if (isEmpty) None
    else if (isLeaf) Some(elems(0))
    else children(0).minimum
  }
  
  def keyList =
    if (isLeaf)
      keys.toList
    else
      ((children.map(_.keyList) zip keys) :\ children(numOfKeys).keyList) ((x, y) => x._1 ::: x._2 :: y)

      
  def validateBTree() {
    // A B Tree should satisfy the following properties.
    // 1. Every node have children of the same type
    // 2. The root & a leaf may contain 4t -1 keys
    // 3. An internal node may have t-1 to 2t-1 keys and # of children should be +1
    
    def validate(node: BTree[T]) {
      node match {
        case BTreeRoot(e, c, leaf, _) =>
          if (leaf) {
            assert(e.length <= 4 * t - 1)
          } else {
            assert(e.length + 1 == c.length)
            for (i <- 1 until c.length) assert(c(0).isLeaf == c(i).isLeaf, println("c(0) = " + c(0) + "\nc" + "(" + i + ") = " + c(i)))
            for (i <- 0 until c.length) validate(c(i))
          }
        case BTreeInternal(e, c, _) =>
          assert(e.length >= t - 1 && e.length <= 2 * t - 1)
          assert(e.length + 1 == c.length)
          for (i <- 1 until c.length) assert(c(0).isLeaf == c(i).isLeaf, println("c(0) = " + c(0) + "\nc" + "(" + i + ") = " + c(i)))
          for (i <- 0 until c.length) validate(c(i))
        case BTreeLeaf(e, _) =>
          assert(e.length >= 2 * t - 1 && e.length <= 4 * t - 1)
      }
    }
    
    validate(this)
  }

  def insert(e: Element[T]): BTree[T]
  def delete(e: Element[T]): BTree[T]
  
  // utils for insert and delete
  protected[btree] def splittable: Boolean
  protected[btree] def mergeable: Boolean
  protected[btree] def median: Int
  
  protected[btree] def createNode(e: Vector[Element[T]], c: Vector[BTree[T]], l: Boolean): BTree[T]
  protected[btree] def createChildNode(e: Vector[Element[T]], c: Vector[BTree[T]]): BTree[T] = {
    require(!isLeaf)
    if (children(0).isLeaf) BTreeLeaf(e, t)
    else BTreeInternal(e, c, t)
    
  }

  // deal with the root and split and merge
  private def promoteToRoot(): BTree[T] = BTreeRoot(elems, children, isLeaf, t)
  private def demoteFromRoot(): BTree[T] = {
    require(isRoot)
    if (isLeaf) BTreeLeaf(elems, t)
    else BTreeInternal(elems, children, t)
  }


  protected[btree] def split(): (Element[T], BTree[T], BTree[T]) = {  // invoked for the node to split
    require(splittable)
    // the median key is promoted as a pivot and the node is split into two nodes
    
    val left = createNode(elems.slice(0, median), children.slice(0, median+1), isLeaf)
    val right = createNode(elems.slice(median+1, numOfKeys), children.slice(median+1, numOfChildren), isLeaf)
    
    if (isRoot) (elems(median), left.demoteFromRoot(), right.demoteFromRoot())
    else (elems(median), left, right)
  }

  protected[btree] def merge(that: BTree[T], pivot: Element[T]): BTree[T] = {
    require(mergeable && that.mergeable)
    require(elems(numOfKeys-1).key <= pivot.key && pivot.key <= that.elems(0).key)
    require(!isRoot)

    createNode(elems ++ (pivot +: that.elems), children ++ that.children, isLeaf)
  }
  
  protected[btree] def mergeOrStuff(idx: Int): BTree[T] = {
    require(!isLeaf && !mergeable)
    val child = children(idx)
    if (idx > 0 && !children(idx-1).mergeable) {
      // stuff idx'th child with key(idx) and promote the maximum of (idx-1)th child into this node
      val prevChild = children(idx-1)
      val newPrevChild = createChildNode(
          prevChild.elems.slice(0, prevChild.numOfKeys-1),
          prevChild.children.slice(0, prevChild.numOfKeys)
          )
      val newChild = createChildNode(
          elems(idx-1) +: child.elems,
          if (child.isLeaf) Vector[BTree[T]]()
          else prevChild.children(prevChild.numOfKeys) +: child.children
          )
            
      createNode(
          elems.slice(0, idx-1) ++ (prevChild.elems(prevChild.numOfKeys-1) +: elems.slice(idx, numOfKeys)),
          children.slice(0, idx-1) ++ (newPrevChild +: newChild +: children.slice(idx+1, numOfChildren)),
          false
          )
    } else if (idx < numOfKeys && !children(idx+1).mergeable) {
      val nextChild = children(idx+1)
      val newChild = createChildNode(
          child.elems :+ elems(idx),
          if (child.isLeaf) Vector[BTree[T]]()
          else child.children :+ nextChild.children(0)
          )
//      println("this = " + this)
//      println("idx = " + idx)
//      println("child = " + child + " nextChild = " + nextChild + " (" + nextChild.mergeable + ")")
      val newNextChild = createChildNode(
          nextChild.elems.slice(1, nextChild.numOfKeys),
          nextChild.children.slice(1, nextChild.numOfChildren)
          )

      createNode(
          elems.slice(0, idx) ++ (nextChild.elems(0) +: elems.slice(idx+1, numOfKeys)),
          children.slice(0, idx) ++ (newChild +: newNextChild +: children.slice(idx+2, numOfChildren)),
          false
          )
    } else {    // merge two children
      val child = children(idx)
      val newChild =
        if (idx > 0) children(idx-1).merge(children(idx), elems(idx-1))
        else children(idx).merge(children(idx+1), elems(idx))
      
      if (numOfKeys == 1) newChild.promoteToRoot()    // the tree level decreases by 1
      else if (idx > 0)
        createNode(
            elems.slice(0, idx-1) ++ elems.slice(idx, numOfKeys),
            children.slice(0, idx-1) ++ (newChild +: children.slice(idx+1, numOfChildren)),
            false
            )
      else
        createNode(
            elems.slice(0, idx) ++ elems.slice(idx+1, numOfKeys),
            children.slice(0, idx) ++ (newChild +: children.slice(idx+2, numOfChildren)),
            false
            )
    }
  }

  protected[btree] def selectMaximum(): (BTree[T], Element[T]) = {
    require(!mergeable)
    if (isLeaf)
      (createNode(elems.slice(0, numOfKeys-1), Vector[BTree[T]](), true), elems(numOfKeys-1))
    else {
      // if the child to visit is mergeable, then merge or stuff the child
      val cur = if (children(numOfKeys).mergeable) mergeOrStuff(numOfKeys) else this
      val (child, e) = cur.children(cur.numOfKeys).selectMaximum()
      
      (createNode(cur.elems, cur.children.slice(0, cur.numOfKeys) :+ child, false), e)
    }
  }

  protected[btree] def selectMinimum(): (BTree[T], Element[T]) = {
    require(!mergeable)
    if (isLeaf)
      (createNode(elems.slice(1, numOfKeys), Vector[BTree[T]](), true), elems(0))
    else {
      // if the child to visit is mergeable, then merge or stuff the child
      // if the child to visit is mergeable, then merge or stuff the child
      val cur = if (children(0).mergeable) mergeOrStuff(0) else this
      val (child, e) = cur.children(0).selectMinimum()
      
      (createNode(cur.elems, child +: cur.children.slice(1, cur.numOfChildren), false), e)
    }    
  }

  // idx is a key index, not a child index
  // if return -1, then left has been used for replacement
  // if return 1, then right has been used
  // if return 0, then just merge with the key
  protected[btree] def createReplacement(idx: Int): (BTree[T], Element[T], Int) = {
    require(!isLeaf)
    if (!children(idx).mergeable) {
      // select the maximum in the left child as the replacement
      val (t, e) = children(idx).selectMaximum()
      (t, e, -1)
    } else if (!children(idx+1).mergeable) {
      val (t, e) = children(idx+1).selectMinimum()
      (t, e, 1)
    } else {    // merge the two nodes into one with the key
      var mergedChild = children(idx).merge(children(idx+1), elems(idx))
      if (numOfKeys == 1) mergedChild = mergedChild.promoteToRoot()
      (mergedChild, null, 0)
    }
  }
}

// root may have less than (t-1) keys
case class BTreeRoot[T: Ordering](
    elems: Vector[Element[T]],
    children: Vector[BTree[T]],
    isLeaf: Boolean,
    t: Int) extends BTree[T] {
  def splittable() = numOfKeys == (if (isLeaf) 4 * t - 1 else 2 * t - 1)
  def mergeable() = false
  def median = if (isLeaf) 2 * t - 1 else t - 1
  override def isEmpty = numOfKeys == 0
  def isRoot = true
  
  def createNode(e: Vector[Element[T]], c: Vector[BTree[T]], l: Boolean): BTree[T] = BTreeRoot(e, c, l, t)
      
  def insert(e: Element[T]): BTree[T] = {
    if (splittable) {
      // only consider root splitting
      val (pivot, left, right) = split()
      createNode(Vector(pivot), Vector(left, right), false).insert(e)
    } else if (isLeaf) {
      val (idx, _) = findIndex(e.key)
      val newElems = elems.slice(0, idx) ++ (e +: elems.slice(idx, elems.length))
      createNode(newElems, Vector[BTree[T]](), true)            
    } else {
      // now, assume that the current node has a room for the new element.
      val (idx, _) = findIndex(e.key)
      // the new element should be put into children(idx)
      // split the node if necessary
      if (children(idx).splittable) {
        val (pivot, left, right) = children(idx).split()
        
        // create a new root
        val newElems = elems.slice(0, idx) ++ (pivot +: elems.slice(idx, elems.length))
        val newChildren =
          if (e.key < pivot.key) {
            val newLeft = left.insert(e)
            children.slice(0, idx) ++ (newLeft +: right +: children.slice(idx+1, children.length))
          } else {
            val newRight = right.insert(e)
            children.slice(0, idx) ++ (left +: newRight +: children.slice(idx+1, children.length))
          }
        createNode(newElems, newChildren, false)
      } else {
        val newChildren =
          children.slice(0, idx) ++ ((children(idx).insert(e)) +: children.slice(idx+1, children.length))
        createNode(elems, newChildren, false)
      }
    }
  }
  
  def delete(e: Element[T]): BTree[T] = {
    require(!mergeable)
    
    val (idx, found) = findIndex(e.key)

    if (isLeaf)
      if (found)
        createNode(elems.slice(0, idx) ++ elems.slice(idx+1, elems.length), Vector[BTree[T]](), true)
      else this
    else if (found) {
      // find a replacement or remove this with merging left and right children into one
      val (child, r, flag) = createReplacement(idx)
      flag match {
        case -1 =>
          createNode(elems.slice(0, idx) ++ (r +: elems.slice(idx+1, numOfKeys)),
              children.slice(0, idx) ++ (child +: children.slice(idx+1, numOfChildren)),
              false)
        case 1 =>
          createNode(elems.slice(0, idx) ++ (r +: elems.slice(idx+1, numOfKeys)),
              children.slice(0, idx+1) ++ (child +: children.slice(idx+2, numOfChildren)),
              false)
        case 0 =>
          // replacement has not been found. the two children and the key has been merged into one.
          val newChild = child.delete(e)
          if (!newChild.isRoot)
            createNode(elems.slice(0, idx) ++ elems.slice(idx+1, numOfKeys),
                children.slice(0, idx) ++ (newChild +: children.slice(idx+2, numOfChildren)),
                false)
          else newChild
      }
    } else {
      // if the child to visit is mergeable, then merge or stuff with elem(idx)
      if (children(idx).mergeable) mergeOrStuff(idx).delete(e)
      else {
        val res = children(idx).delete(e)
        if (children(idx) != res)
          createNode(elems,
              children.slice(0, idx) ++ (res +: children.slice(idx+1, numOfChildren)), false)
        else this
      }
    }
  }

}

// internal node may have (t-1) to (2t-1) keys
case class BTreeInternal[T : Ordering](
    elems: Vector[Element[T]],
    children: Vector[BTree[T]],
    t: Int) extends BTree[T] {
  def isLeaf = false
  def isRoot = false
  def splittable = numOfKeys == (2 * t - 1)
  def mergeable = numOfKeys == (t - 1)
  def median = t - 1
  
  def createNode(e: Vector[Element[T]], c: Vector[BTree[T]], l: Boolean): BTree[T] = BTreeInternal(e, c, t)
  
  def insert(e: Element[T]): BTree[T] = {
    // now, assume that the current node has a room for the new element.
    require(!splittable)
    val (idx, _) = findIndex(e.key)
    // the new element should be put into children(idx)
    // split the node if necessary
    if (children(idx).splittable) {
      val (pivot, left, right) = children(idx).split()
      
      // create a new internal node
      val newElems = elems.slice(0, idx) ++ (pivot +: elems.slice(idx, elems.length))
      val newChildren =
        if (e.key < pivot.key) {
          val newLeft = left.insert(e)
          children.slice(0, idx) ++ (newLeft +: right +: children.slice(idx+1, children.length))
        } else {
          val newRight = right.insert(e)
          children.slice(0, idx) ++ (left +: newRight +: children.slice(idx+1, children.length))
        }
      createNode(newElems, newChildren, false)
    } else {
      val newChildren =
        children.slice(0, idx) ++ ((children(idx).insert(e)) +: children.slice(idx+1, children.length))
      createNode(elems, newChildren, false)
    }
  }
  
  def delete(e: Element[T]): BTree[T] = {
    val (idx, found) = findIndex(e.key)
    
    if (found) {
      // find a replacement or remove this with merging left and right children into one
      val (child, r, flag) = createReplacement(idx)
      flag match {
        case -1 =>
          createNode(elems.slice(0, idx) ++ (r +: elems.slice(idx+1, numOfKeys)),
              children.slice(0, idx) ++ (child +: children.slice(idx+1, numOfChildren)), false)
        case 1 =>
          createNode(elems.slice(0, idx) ++ (r +: elems.slice(idx+1, numOfKeys)),
              children.slice(0, idx+1) ++ (child +: children.slice(idx+2, numOfChildren)), false)
        case 0 =>
          // replacement has not been found. the two children and the key has been merged into one.
          val newChild = child.delete(e)
          createNode(elems.slice(0, idx) ++ elems.slice(idx+1, numOfKeys),
              children.slice(0, idx) ++ (newChild +: children.slice(idx+2, numOfChildren)), false)
      }
    } else {
      // if the child to visit is mergeable, then merge or stuff with elem(idx)
      if (children(idx).mergeable) mergeOrStuff(idx).delete(e)
      else {
        val res = children(idx).delete(e)
        if (children(idx) != res)
          createNode(elems,
              children.slice(0, idx) ++ (res +: children.slice(idx+1, numOfChildren)), false)
        else this
      }
    }
  }
}

// leaf node does has no child. Instead, it can store double size of keys. (t-1 to 4t-1) 
case class BTreeLeaf[T: Ordering](elems: Vector[Element[T]], t: Int) extends BTree[T] {
  override def keyList = keys.toList

  def children = Vector[BTree[T]]()
  def isLeaf = true
  def isRoot = false
  def splittable = numOfKeys == (4 * t - 1)
  def mergeable = numOfKeys == (2 * t - 1)
  def median = 2 * t - 1

  def createNode(e: Vector[Element[T]], c: Vector[BTree[T]], l: Boolean): BTree[T] = BTreeLeaf(e, t)

  def insert(e: Element[T]): BTree[T] = {
    // now, assume that the current node has a room for the new element.
    require(!splittable)
    val (idx, _) = findIndex(e.key)
    // the new element should be put into at idx
    val newElems = elems.slice(0, idx) ++ (e +: elems.slice(idx, elems.length))
    createNode(newElems, Vector[BTree[T]](), true)            
  }
  
  def delete(e: Element[T]): BTree[T] = {
    require(!mergeable)
    
    val (idx, found) = findIndex(e.key)
//    println("num of keys = " + numOfKeys)
//    println("node = " + this)
//    println("e = " + e + " idx = " + idx + " found = " + found)
//    println(elems.slice(0, idx) ++ elems.slice(idx+1, numOfKeys))
    if (found) createNode(elems.slice(0, idx) ++ elems.slice(idx+1, numOfKeys), Vector[BTree[T]](), true)
    else this
  }
  
  assert(elems != Vector[Element[T]]())
}



object BTree {
  def build[T : Ordering](xs: List[Element[T]], t: Int): BTree[T] = {
    val emptyTree: BTree[T] = BTreeRoot[T](Vector[Element[T]](), Vector[BTree[T]](), true, t)
    (emptyTree /: xs) { _.insert(_) }
  }
}