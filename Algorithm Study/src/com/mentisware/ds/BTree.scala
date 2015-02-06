package com.mentisware.ds

// immutable B Tree dynamic set
// leaf nodes may have twice more data than internal nodes by utilizing the child pointers.

trait WorkLoad[T] {
  def key: T
  override def toString = key.toString
}

abstract class BTree {
  val t: Int    // Except the root node, a node can have (t-1) to (2t-1) keys, that is, t to 2t children.
  
}

object BTree {
  
}