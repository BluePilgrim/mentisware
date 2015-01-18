package com.mentisware.ds

class OrderingTree[T : Ordering, E <: Node[T]](
    elem: E,
    left: RBTree[T, E],
    right: RBTree[T, E],
    color: Color
    ) extends NormalTree[T, E](elem, left, right, color) {
  override val size = left.size + right.size + 1
  override def createNode(e: E, l: RBTree[T, E], r: RBTree[T, E], c: Color): RBTree[T, E] = new OrderingTree(e, l, r, c)
}


object OrderingTree {
  def build[T : Ordering, E <: Node[T]](xs: List[E]): RBTree[T, E] = {
    if (xs.length == 0) NilTree
    else {
      val tree: RBTree[T, E] = new OrderingTree[T, E](xs.head, NilTree, NilTree, Black)
      
      (tree /: xs.tail) { _.insert(_) }
    }
  }
}