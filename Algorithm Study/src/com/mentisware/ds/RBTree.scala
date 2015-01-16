package com.mentisware.ds

import Ordering.Implicits._

sealed abstract class Color
case object Red extends Color
case object Black extends Color

trait Node[T] {
  def key: T
  override def toString = key.toString
}

abstract class RBTree[+T, +Elem] {
  def elem: Elem
  def left: RBTree[T, Elem]
  def right: RBTree[T, Elem]
  def key : T
  
  def predecessor: RBTree[T, Elem]
  def successor: RBTree[T, Elem]
  def search[A >: T : Ordering](k: A): RBTree[T, Elem]
  def minimum: Option[Elem]
  def maximum: Option[Elem]
  def insert[B >: Elem](n: B): RBTree[T, Elem]
  def delete[B >: Elem](n: B): RBTree[T, Elem]

  def color: Color
  def isEmpty(): Boolean

  def error(m: String) = throw new NoSuchElementException(m)
  
  override def toString = "[" + left.toString + " (" + elem + ") " + right.toString + "]"
}


case object NilTree extends RBTree[Nothing, Nothing] {
  def color = Black
  def isEmpty = true
  
  def elem = error("NilTree.elem")
  def left = error("NilTree.left")
  def right = error("NilTree.right")
  def key = error("NilTree.key")
  
  def predecessor = error("NilTree.predecessor")
  def successor = error("NilTree.successor")
  def search[A >: Nothing : Ordering](k: A) = this
  def minimum = None
  def maximum = None
  def insert[B >: Nothing](n: B) = error("NilTree.insert")
  def delete[B >: Nothing](n: B) = error("NilTree.delete")
  
  override def toString = ""
}

case class NormalTree[T : Ordering, E <: Node[T]](
    elem: E,
    left: RBTree[T, E],
    right: RBTree[T, E],
    color: Color
    ) extends RBTree[T, E] {
  def isEmpty = false
  def key = elem.key

  /* internal utility methods */  
  private def findMaximum(node: RBTree[T, E]): RBTree[T, E] = node match {
    case NilTree => this
    case _ => if (node.right.isEmpty) node else findMaximum(node.right)
  }

  private def findMinimum(node: RBTree[T, E]): RBTree[T, E] = node match {
    case NilTree => this
    case _ => if (node.left.isEmpty) node else findMinimum(node.left)
  }

  
  def extractElem[B >: E](n: B): E = n match {
      case x: E => x
      case _ => error("[insert] unsupported data")
  }

  def predecessor = findMaximum(left)
  
  def successor = findMinimum(right)
  
  def minimum = findMinimum(this) match {
    case NilTree => None
    case node: NormalTree[_, _] => Some(node.elem)
  }
  
  def maximum = findMaximum(this) match {
    case NilTree => None
    case node: NormalTree[_, _] => Some(node.elem)
  }
  
  def search[A >: T : Ordering](k: A) = {
    def _search(node: RBTree[T, E]): RBTree[T, E] =
      if (k < node.key) _search(node.left)
      else if (k > node.key) _search(node.right)
      else node
      
    _search(this)
  }


  def insert[B >: E](n: B): RBTree[T, E] = {
    val data = extractElem(n)
    val k = data.key
    
    def _insert(node: RBTree[T, E]): RBTree[T, E] = {
      if (k < node.key) {
        if (node.left.isEmpty) {  // new node is attached to the tree
          val baby = NormalTree[T, E](data, NilTree, NilTree, Red)
          NormalTree(node.elem, baby, node.right, node.color)
        } else {        
          NormalTree(node.elem, _insert(node.left), node.right, node.color)
        }
      } else {
        if (node.right.isEmpty) {  // new node is attached to the tree
          val baby = NormalTree[T, E](data, NilTree, NilTree, Red)
          NormalTree(node.elem, node.left, baby, node.color)
        } else {
          NormalTree(node.elem, node.left, _insert(node.right), node.color)
        }
      }
    }
    
    _insert(this)
  }

  def delete[B >: E](n: B): RBTree[T, E] = {
    val data = extractElem(n)
    val k = data.key

    def _delete(node: RBTree[T, E]): RBTree[T, E] = {
      if (k < node.key)      // if the left is NilTree, exception will be raised.
        NormalTree(node.elem, _delete(node.left), node.right, node.color)
      else if (k > node.key)
        NormalTree(node.elem, node.left, _delete(node.right), node.color)
      else {
        // the node to delete found.
        // select the replacer with the successor for the time being
        if (node.right.isEmpty) node.left
        else {
          val (baby, replacer) = getReplacer(node.right, true)
          NormalTree(replacer.elem, node.left, baby, replacer.color)
        }
      }
    }
    
    def getReplacer(node: RBTree[T, E], withSuccessor: Boolean): (RBTree[T, E], RBTree[T, E]) = {
      if (!withSuccessor) error("Not implemented yet")
      else {
        if (node.left.isEmpty) (node.right, node)
        else {
          val (newNode, replacer) = getReplacer(node.left, withSuccessor)
          (NormalTree(node.elem, newNode, node.right, node.color), replacer)
        }
      }
    }
    
    _delete(this)
  }
}
