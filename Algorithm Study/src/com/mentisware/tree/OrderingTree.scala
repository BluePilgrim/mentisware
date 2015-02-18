package com.mentisware.tree

class OrderingTree[T : Ordering, E <: Node[T]](
    elem: E,
    left: RBTree[T, E],
    right: RBTree[T, E],
    color: Color,
    nil: NilTree[T, E]
    ) extends NormalTree[T, E](elem, left, right, color, nil) {
  override val size = left.size + right.size + 1
  override def createNode(e: E, l: RBTree[T, E], r: RBTree[T, E], c: Color): RBTree[T, E] = new OrderingTree(e, l, r, c, nil)
}


object OrderingTree {
  def build[T : Ordering, E <: Node[T]](xs: List[E]): RBTree[T, E] = {
    val nil = new NilTree[T, E]
    if (xs.length == 0) nil
    else {
      val tree: RBTree[T, E] = new OrderingTree[T, E](xs.head, nil, nil, Black, nil)
      
      (tree /: xs.tail) { _.insert(_) }
    }
  }
}