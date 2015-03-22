package com.mentisware.tree.rbtree

import com.mentisware.tree._

class OrderingTree[T : Ordering](
    elem: Element[T],
    left: RBTree[T],
    right: RBTree[T],
    color: Color,
    nil: NilTree[T]
    ) extends NormalTree[T](elem, left, right, color, nil) {
  override val size = left.size + right.size + 1
  override def createNode(e: Element[T], l: RBTree[T], r: RBTree[T], c: Color): RBTree[T] = new OrderingTree(e, l, r, c, nil)
}


object OrderingTree {
  def build[T : Ordering](xs: Seq[Element[T]]): RBTree[T] = {
    val nil = new NilTree[T]
    if (xs.length == 0) nil
    else {
      val tree: RBTree[T] = new OrderingTree[T](xs.head, nil, nil, Black, nil)
      
      (tree /: xs.tail) { _.insert(_) }
    }
  }
}