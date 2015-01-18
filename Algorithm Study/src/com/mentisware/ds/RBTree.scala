package com.mentisware.ds

import Ordering.Implicits._

sealed abstract class Color
case object Red extends Color
case object Black extends Color

trait Node[T] {
  def key: T
  override def toString = key.toString
}

trait RBTree[+T, +Elem] {
  def elem: Elem
  def left: RBTree[T, Elem]
  def right: RBTree[T, Elem]
  def key : T
  
  def predecessor: RBTree[T, Elem]
  def successor: RBTree[T, Elem]
  def search[A >: T : Ordering](k: A): RBTree[T, Elem] = {
    if (k == key) this
    else if (k < key) left.search(k)
    else right.search(k)
  }

  def minimum: Option[Elem]
  def maximum: Option[Elem]
  def insert[B >: Elem](n: B): RBTree[T, B]
  def delete[B >: Elem](n: B): RBTree[T, B]

  def color: Color
  def isEmpty(): Boolean

  def keyList(): List[T] = left.keyList ::: key :: right.keyList
    
  def validateRBProperty() {
    // A Red Black Tree should satisfy the following properties.
    // 1. Every node is either red or black
    // 2. The root is black. (optional, but it is necessary for easy implementation
    // 3. Every leaf(NIL) is black
    // 4. If a node is red, then both its children are black
    // 5. For each node, all simple paths from the node to descendant leaves contain the same number of black nodes
    
    if (color == Red) error("The root is red")
    
    def validate(node: RBTree[T, Elem]): Int = {
      val leftBH = if (node.left.isEmpty) 1 else validate(node.left)
      val rightBH = if (node.right.isEmpty) 1 else validate(node.right)
      
      if (leftBH != rightBH) error("Different BH at " + node + ": left BH = " + leftBH + " right BH = " + rightBH)
      
      if (node.color == Red) {
        if (node.left.color == Red || node.right.color == Red)
          error("Node " + node.elem + " has the red conflict" + "\n" + node + "\nThe tree = " + this)
        leftBH
      } else leftBH + 1
    }
    
    val treeBH = validate(this)
//    println("tree BH = " + treeBH)
  }

  def error(m: String) = throw new NoSuchElementException(m)
  
  override def toString =
    "[" + left.toString + " (" + elem + ")" + (if (color == Black) "B " else "R ") + right.toString + "]"
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
  override def search[A >: Nothing : Ordering](k: A) = this
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
  private def findMaximum(): RBTree[T, E] = right match {
    case NilTree => this
    case node: NormalTree[T, E] => node.findMaximum()
  }

  private def findMinimum(): RBTree[T, E] = left match {
    case NilTree => this
    case node: NormalTree[T, E] => node.findMinimum()
  }

  
  def extractElem[B >: E](n: B): E = n match {
      case x: E => x
      case _ => error("[Normal Tree] unsupported data")
  }

  def predecessor = left match {
    case NilTree => NilTree
    case node: NormalTree[T, E] => node.findMaximum()
  }
  
  def successor = right match {
    case NilTree => NilTree
    case node: NormalTree[T, E] => node.findMinimum()
  }
  
  def minimum = findMinimum() match {
    case NilTree => None
    case node: NormalTree[_, _] => Some(node.elem)
  }
  
  def maximum = findMaximum() match {
    case NilTree => None
    case node: NormalTree[_, _] => Some(node.elem)
  }

/*
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
*/

  private def checkRedConflict(node: RBTree[T, E]): Int = {
    // check if one of its child is also RED in case of a RED node
    // I define a stale state as requiring some fix should be done via some rotation,
    // and a stable state as the fixation has been done.
    // At stale state
    //     the newly inserted node set the flag 1
    //     the parent calls "checkRedConflict" and set the flag as follows
    //         . no conflict or its color is black) : 0 while exiting the state state
    //         . left child is Red : 2
    //         . right child is Red : 3
    //
    if (node.color == Red) {
      assert(!(node.left.color == Red && node.right.color == Red))
      if (node.left.color == Red) 2
      else if (node.right.color == Red) 3
      else 0
    } else 0
  }

  private def resolveRedConflict(
      node: RBTree[T, E],
      conflictFlag: Int,
      isLeftChild: Boolean): (RBTree[T, E], Int) = {
    assert(node.color == Black)

    if (isLeftChild) {
      conflictFlag match {
        case 2 =>    // there is red conflict between left child and the left grandchild
          if (node.right.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = NormalTree(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = NormalTree(node.right.elem, node.right.left, node.right.right, Black)
            (NormalTree(node.elem, newLeft, newRight, Red), 1)  // now stale
          } else {
            // right rotate on this node and set the original red child black while this node red
//              val a = node.left.left.left
//              val b = node.left.left.right
            val c = node.left.right
            val d = node.right
            
            val newRight= NormalTree(node.elem, c, d, Red)
            (NormalTree(node.left.elem, node.left.left, newRight, Black), 0)  // now stable
          }
        case 3 =>    // there is red conflict between left child and the right grandchild
          if (node.right.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = NormalTree(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = NormalTree(node.right.elem, node.right.left, node.right.right, Black)                
            (NormalTree(node.elem, newLeft, newRight, Red), 1)
          } else {
            // the grandchild is set to the new node and this node becomes the right child
            val a = node.left.left
            val b = node.left.right.left
            val c = node.left.right.right
            val d = node.right
            
            val newLeft = NormalTree(node.left.elem, a, b, Red)
            val newRight = NormalTree(node.elem, c, d, Red)
            (NormalTree(node.left.right.elem, newLeft, newRight, Black), 0)
          }
        case _ => error("Strange conflict flag")
      }
    } else {
      conflictFlag match {
        case 2 =>    // there is red conflict between right child and the left grandchild
          assert(node.color == Black)
          if (node.left.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = NormalTree(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = NormalTree(node.right.elem, node.right.left, node.right.right, Black)
            (NormalTree(node.elem, newLeft, newRight, Red), 1)  // now stale
          } else {
            // the grandchild is set to the new node and this node becomes the left child
            val a = node.left
            val b = node.right.left.left
            val c = node.right.left.right
            val d = node.right.right
            
            val newLeft = NormalTree(node.elem, a, b, Red)
            val newRight = NormalTree(node.right.elem, c, d, Red)
            (NormalTree(node.right.left.elem, newLeft, newRight, Black), 0)
          }
        case 3 =>    // there is red conflict between right child and the right grandchild
          if (node.left.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = NormalTree(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = NormalTree(node.right.elem, node.right.left, node.right.right, Black)                
            (NormalTree(node.elem, newLeft, newRight, Red), 1)
          } else {
            // left rotate on this node and set the original red child black while this node red
            val a = node.left
            val b = node.right.left
//              val c = node.right.right.left
//              val d = node.right.right.right
            
            val newLeft = NormalTree(node.elem, a, b, Red)
            (NormalTree(node.right.elem, newLeft, node.right.right, Black), 0)  // now stable
          }
        case _ => error("Strange conflict flag")
      }
    }
  }

  def insert[B >: E](n: B): RBTree[T, B] = {
    val data = extractElem(n)
    val k = data.key

    require(this.color == Black)    // for implementation convenience, I follow CLR's RB Tree definition.
    
    def _insert(node: RBTree[T, E]): (RBTree[T, E], Int) = {
      if (k < node.key) {
        if (node.left.isEmpty) {  // new node is attached to the tree
          val baby = NormalTree[T, E](data, NilTree, NilTree, Red)
          val newNode = NormalTree(node.elem, baby, node.right, node.color)
          (newNode, checkRedConflict(newNode))    // Already in stale, so check the conflict
        } else {
          val (newLeft, conflictFlag) = _insert(node.left)
          val newNode = NormalTree(node.elem, newLeft, node.right, node.color)
          conflictFlag match {
            case 0 =>      // no color fixation is further required.
              (newNode, 0)
            case 1 =>      // in stale mode, check is required.
              (newNode, checkRedConflict(newNode))
            case _ =>      // some resolve is required.
              resolveRedConflict(newNode, conflictFlag, true)
          }
        }
      } else {
        if (node.right.isEmpty) {  // new node is attached to the tree
          val baby = NormalTree[T, E](data, NilTree, NilTree, Red)
          val newNode = NormalTree(node.elem, node.left, baby, node.color)
          (newNode, checkRedConflict(newNode))    // already in stale, so check the conflict
        } else {
          val (newRight, conflictFlag) = _insert(node.right)
          val newNode = NormalTree(node.elem, node.left, newRight, node.color)
          conflictFlag match {
            case 0 =>      // no color fixation is required.
              (newNode, 0)
            case 1 =>      // in stale mode, check is required.
              (newNode, checkRedConflict(newNode))
            case _ =>      // some resolve is required.
              resolveRedConflict(newNode, conflictFlag, false)
          }
        }
      }
    }

    val (root, conflictFlag) = _insert(this)

    // conflict flag is negligible
    if (root.color == Red) NormalTree(root.elem, root.left, root.right, Black) else root
  }


  def resolveBlackConflict(node: RBTree[T, E], isLeftChild: Boolean): (RBTree[T, E], Boolean) = {
    if (isLeftChild) {
      require(node.left.color == Black)
      val x = node.left
      val p = node
      val s = node.right
      val y = s.left
      val z = s.right
      
      s.color match {
        case Black if z.color == Red =>
          val newP = NormalTree(p.elem, x, y, Black)
          val newZ = NormalTree(z.elem, z.left, z.right, Black)
          (NormalTree(s.elem, newP, newZ, p.color), false)
          
        case Black if y.color == Red =>
          val newP = NormalTree(p.elem, x, y.left, Black)
          val newS = NormalTree(s.elem, y.right, z, Black)
          (NormalTree(y.elem, newP, newS, p.color), false)
          
        case Black =>
          val newS = NormalTree(s.elem, y, z, Red)
          (NormalTree(p.elem, x, newS, Black), p.color == Black)
          
        case Red =>
          val a = y.left
          val b = y.right
          if (b.color == Red) {
            val newB = NormalTree(b.elem, b.left, b.right, Black)
            val newP = NormalTree(p.elem, x, a, Black)
            val newY = NormalTree(y.elem, newP, newB, Red)
            (NormalTree(s.elem, newY, z, Black), false)
          } else if (a.color == Red) {
            val newP = NormalTree(p.elem, x, a.left, Black)
            val newY = NormalTree(y.elem, a.right, b, Black)
            val newA = NormalTree(a.elem, newP, newY, Red)
            (NormalTree(s.elem, newA, z, Black), false)
          } else {
            val newY = NormalTree(y.elem, a, b, Red)
            val newP = NormalTree(p.elem, x, newY, Black)
            (NormalTree(s.elem, newP, z, Black), false)
          }
      }
    } else {
      require(node.right.color == Black)

      val p = node
      val x = node.right
      val s = node.left
      val y = s.left
      val z = s.right
      
      s.color match {
        case Black if y.color == Red =>
          val newP = NormalTree(p.elem, z, x, Black)
          val newY = NormalTree(y.elem, y.left, y.right, Black)
          (NormalTree(s.elem, newY, newP, p.color), false)
          
        case Black if z.color == Red =>
          val newP = NormalTree(p.elem, z.right, x, Black)
          val newS = NormalTree(s.elem, y, z.left, Black)
          (NormalTree(z.elem, newS, newP, p.color), false)
          
        case Black =>
          val newS = NormalTree(s.elem, y, z, Red)
          (NormalTree(p.elem, newS, x, Black), p.color == Black)
          
        case Red =>
          val a = z.left
          val b = z.right
          if (a.color == Red) {
            val newA = NormalTree(a.elem, a.left, a.right, Black)
            val newP = NormalTree(p.elem, b, x, Black)
            val newZ = NormalTree(z.elem, newA, newP, Red)
            (NormalTree(s.elem, y, newZ, Black), false)
          } else if (b.color == Red) {
            val newP = NormalTree(p.elem, b.right, x, Black)
            val newZ = NormalTree(z.elem, a, b.left, Black)
            val newB = NormalTree(b.elem, newZ, newP, Red)
            (NormalTree(s.elem, y, newB, Black), false)
          } else {
            val newZ = NormalTree(z.elem, a, b, Red)
            val newP = NormalTree(p.elem, newZ, x, Black)
            (NormalTree(s.elem, y, newP, Black), false)
          }
      }
    }
  }

  def delete[B >: E](n: B): RBTree[T, B] = {
    val data = extractElem(n)
    val k = data.key
    
    require(this.color == Black)

    def _delete(node: RBTree[T, E]): (RBTree[T, E], Boolean) = {
      if (k < node.key) {     // if the left is NilTree, exception will be raised.
        val (newLeft, isStale) = _delete(node.left)
        val newNode = NormalTree(node.elem, newLeft, node.right, node.color)

        if (isStale) resolveBlackConflict(newNode, true)
        else (newNode, false)
      } else if (k > node.key) {
        val (newRight, isStale) = _delete(node.right)
        val newNode = NormalTree(node.elem, node.left, newRight, node.color)

        if (isStale) resolveBlackConflict(newNode, false)
        else (newNode, false)
      } else {
        // the node to delete found.
        if (node.right.isEmpty || node.left.isEmpty) {
          val replacer = if (node.right.isEmpty) node.left else node.right
          if (node.color == Black) {
            if (replacer.color == Red) (NormalTree(replacer.elem, replacer.left, replacer.right, Black), false)
            else (replacer, true)
          } else (replacer, false)
        } else {    // Neither of two children is empty
          // select the replacer with the successor for the time being
          val (newRight, replacer, isStale) = getReplacer(node.right, true)
          val newNode = NormalTree(replacer.elem, node.left, newRight, node.color)

          if (isStale) resolveBlackConflict(newNode, false)
          else (newNode, false)
        }
      }
    }
    
    def getReplacer(node: RBTree[T, E], withSuccessor: Boolean): (RBTree[T, E], RBTree[T, E], Boolean) = {
      if (!withSuccessor) error("Not implemented yet")
      else {
        if (node.left.isEmpty)    // the successor is selected as the replacer.
          if (node.color == Red) (node.right, node, false)
          else if (node.right.color == Red)
            (NormalTree(node.right.elem, node.right.left, node.right.right, Black), node, false)
          else (node.right, node, true)
        else {
          val (newLeft, replacer, isStale) = getReplacer(node.left, withSuccessor)
          val newNode = NormalTree(node.elem, newLeft, node.right, node.color)
          
          if (isStale) {
              val (n, s) = resolveBlackConflict(newNode, true)
              (n, replacer, s)
          } else (newNode, replacer, false)
        }
      }
    }
    
    val (root, isStale) = _delete(this)

//
//    if (isStale) {
//    }
//    isStale is negligible for the root
//
    if (root.color == Red) NormalTree(root.elem, root.left, root.right, Black)
    else root
  }
}


object RBTree {
  def build[T : Ordering, E <: Node[T]](xs: List[E]): RBTree[T, E] = {
    if (xs.length == 0) NilTree
    else {
      val tree: RBTree[T, E] = NormalTree[T, E](xs.head, NilTree, NilTree, Black)
      
      (tree /: xs.tail) { _.insert(_) }
    }
  }
}