package com.mentisware.tree.rbtree

import com.mentisware.tree._

import Ordering.Implicits._

sealed abstract class Color
case object Red extends Color
case object Black extends Color

abstract class RBTree[T : Ordering] extends SearchTree[T] {
  def elem: Element[T]
  def left: RBTree[T]
  def right: RBTree[T]
  def key : T
  def size: Int

  def keys = Vector(key)
  def children = Vector(left, right)
  
  def predecessor: RBTree[T]
  def successor: RBTree[T]
  
  def search(k: T): RBTree[T] = {
    if (k == key) this
    else if (k < key) left.search(k)
    else right.search(k)
  }

  def apply(k: T): Element[T] = {
    val node = search(k)
    
    if (node != null) node.elem else error(k + "is not in the tree")
  }

  def minimum: Option[Element[T]]
  def maximum: Option[Element[T]]
  def insert(e: Element[T]): RBTree[T]
  def delete(e: Element[T]): RBTree[T]

  def color: Color
  def isEmpty(): Boolean

  def keyList(): List[T] = left.keyList ::: key :: right.keyList

  def selectSmallest(order: Int): RBTree[T] = {
    if (order == left.size + 1) this
    else if (order <= left.size) left.selectSmallest(order)
    else right.selectSmallest(order - left.size - 1)
  }

  def rankIn(mainTree: RBTree[T]): Int

  def validateRBProperty() {
    // A Red Black Tree should satisfy the following properties.
    // 1. Every node is either red or black
    // 2. The root is black. (optional, but it is necessary for easy implementation
    // 3. Every leaf(NIL) is black
    // 4. If a node is red, then both its children are black
    // 5. For each node, all simple paths from the node to descendant leaves contain the same number of black nodes
    
    if (color == Red) error("The root is red")
    
    def validate(node: RBTree[T]): Int = {
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

  override def toString =
    "[" + left.toString + " (" + elem + ")" + (if (color == Black) "B " else "R ") + right.toString + "]"
}


class NilTree[T : Ordering] extends RBTree[T] {
  def color = Black
  def isEmpty = true
  def size = 0
  
  def elem = error("NilTree.elem")
  def left = error("NilTree.left")
  def right = error("NilTree.right")
  def key = error("NilTree.key")
  
  def predecessor = error("NilTree.predecessor")
  def successor = error("NilTree.successor")
  override def search(k: T) = this

  def rankIn(mainTree: RBTree[T]) = error("NilTree.rankIn")
  
  def minimum = None
  def maximum = None
  def insert(e: Element[T]) = error("NilTree.insert")
  def delete(e: Element[T]) = error("NilTree.delete")
  
  override def toString = ""
}

class NormalTree[T : Ordering](
    val elem: Element[T],
    val left: RBTree[T],
    val right: RBTree[T],
    val color: Color,
    private val nil: NilTree[T]
    ) extends RBTree[T] {
  def isEmpty = false
  def key = elem.key
  def size = left.size + right.size + 1

  // subclasses based on NormalTree should override this methods to do something for their own.
  protected def createNode(e: Element[T], l: RBTree[T], r: RBTree[T], c: Color): RBTree[T] = new NormalTree(e, l, r, c, nil)
  
  /* internal utility methods */  
  private def findMaximum(): RBTree[T] = right match {
    case node: NormalTree[T] => node.findMaximum()
    case _ => this
  }

  private def findMinimum(): RBTree[T] = left match {
    case node: NormalTree[T] => node.findMinimum()
    case _ => this
  }
  
  def predecessor = left match {
    case node: NormalTree[T] => node.findMaximum()
    case _ => nil
  }
  
  def successor = right match {
    case node: NormalTree[T] => node.findMinimum()
    case _ => nil
  }
  
  def minimum = findMinimum() match {
    case node: NormalTree[T] => Some(node.elem)
    case _ => None
  }
  
  def maximum = findMaximum() match {
    case node: NormalTree[T] => Some(node.elem)
    case _ => None
  }

  
  def rankIn(mainTree: RBTree[T]): Int = {
    def rank(m: RBTree[T], offset: Int): Int = {
      if (key == m.key) offset + m.left.size + 1
      else if (key < m.key) rank(m.left, offset)
      else rank(m.right, offset + m.left.size + 1)
    }
    
    rank(mainTree.asInstanceOf[RBTree[T]], 0)
  }

/*
  def insert[B >: E](n: B): RBTree[T] = {
    val data = extractElem(n)
    val k = data.key
    
    def _insert(node: RBTree[T]): RBTree[T] = {
      if (k < node.key) {
        if (node.left.isEmpty) {  // new node is attached to the tree
          val baby = NormalTree[T](data, NilTree, NilTree, Red)
          NormalTree(node.elem, baby, node.right, node.color)
        } else {        
          NormalTree(node.elem, _insert(node.left), node.right, node.color)
        }
      } else {
        if (node.right.isEmpty) {  // new node is attached to the tree
          val baby = NormalTree[T](data, NilTree, NilTree, Red)
          NormalTree(node.elem, node.left, baby, node.color)
        } else {
          NormalTree(node.elem, node.left, _insert(node.right), node.color)
        }
      }
    }
    
    _insert(this)
  }

  def delete[B >: E](n: B): RBTree[T] = {
    val data = extractElem(n)
    val k = data.key

    def _delete(node: RBTree[T]): RBTree[T] = {
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
    
    def getReplacer(node: RBTree[T], withSuccessor: Boolean): (RBTree[T], RBTree[T]) = {
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

  private def checkRedConflict(node: RBTree[T]): Int = {
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
      node: RBTree[T],
      conflictFlag: Int,
      isLeftChild: Boolean): (RBTree[T], Int) = {
    assert(node.color == Black)

    if (isLeftChild) {
      conflictFlag match {
        case 2 =>    // there is red conflict between left child and the left grandchild
          if (node.right.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = createNode(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = createNode(node.right.elem, node.right.left, node.right.right, Black)
            (createNode(node.elem, newLeft, newRight, Red), 1)  // now stale
          } else {
            // right rotate on this node and set the original red child black while this node red
//              val a = node.left.left.left
//              val b = node.left.left.right
            val c = node.left.right
            val d = node.right
            
            val newRight= createNode(node.elem, c, d, Red)
            (createNode(node.left.elem, node.left.left, newRight, Black), 0)  // now stable
          }
        case 3 =>    // there is red conflict between left child and the right grandchild
          if (node.right.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = createNode(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = createNode(node.right.elem, node.right.left, node.right.right, Black)                
            (createNode(node.elem, newLeft, newRight, Red), 1)
          } else {
            // the grandchild is set to the new node and this node becomes the right child
            val a = node.left.left
            val b = node.left.right.left
            val c = node.left.right.right
            val d = node.right
            
            val newLeft = createNode(node.left.elem, a, b, Red)
            val newRight = createNode(node.elem, c, d, Red)
            (createNode(node.left.right.elem, newLeft, newRight, Black), 0)
          }
        case _ => error("Strange conflict flag")
      }
    } else {
      conflictFlag match {
        case 2 =>    // there is red conflict between right child and the left grandchild
          assert(node.color == Black)
          if (node.left.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = createNode(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = createNode(node.right.elem, node.right.left, node.right.right, Black)
            (createNode(node.elem, newLeft, newRight, Red), 1)  // now stale
          } else {
            // the grandchild is set to the new node and this node becomes the left child
            val a = node.left
            val b = node.right.left.left
            val c = node.right.left.right
            val d = node.right.right
            
            val newLeft = createNode(node.elem, a, b, Red)
            val newRight = createNode(node.right.elem, c, d, Red)
            (createNode(node.right.left.elem, newLeft, newRight, Black), 0)
          }
        case 3 =>    // there is red conflict between right child and the right grandchild
          if (node.left.color == Red) {
            // set both children to Black while this node to Red
            val newLeft = createNode(node.left.elem, node.left.left, node.left.right, Black)
            val newRight = createNode(node.right.elem, node.right.left, node.right.right, Black)                
            (createNode(node.elem, newLeft, newRight, Red), 1)
          } else {
            // left rotate on this node and set the original red child black while this node red
            val a = node.left
            val b = node.right.left
//              val c = node.right.right.left
//              val d = node.right.right.right
            
            val newLeft = createNode(node.elem, a, b, Red)
            (createNode(node.right.elem, newLeft, node.right.right, Black), 0)  // now stable
          }
        case _ => error("Strange conflict flag")
      }
    }
  }

  def insert(e: Element[T]): RBTree[T] = {
    val data = e
    val k = data.key

    require(this.color == Black)    // for implementation convenience, I follow CLR's RB Tree definition.
    
    def _insert(node: RBTree[T]): (RBTree[T], Int) = {
      if (k < node.key) {
        if (node.left.isEmpty) {  // new node is attached to the tree
          val baby = createNode(data, nil, nil, Red)
          val newNode = createNode(node.elem, baby, node.right, node.color)
          (newNode, checkRedConflict(newNode))    // Already in stale, so check the conflict
        } else {
          val (newLeft, conflictFlag) = _insert(node.left)
          val newNode = createNode(node.elem, newLeft, node.right, node.color)
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
          val baby = createNode(data, nil, nil, Red)
          val newNode = createNode(node.elem, node.left, baby, node.color)
          (newNode, checkRedConflict(newNode))    // already in stale, so check the conflict
        } else {
          val (newRight, conflictFlag) = _insert(node.right)
          val newNode = createNode(node.elem, node.left, newRight, node.color)
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
    if (root.color == Red) createNode(root.elem, root.left, root.right, Black) else root
  }


  def resolveBlackConflict(node: RBTree[T], isLeftChild: Boolean): (RBTree[T], Boolean) = {
    if (isLeftChild) {
      require(node.left.color == Black)
      val x = node.left
      val p = node
      val s = node.right
      val y = s.left
      val z = s.right
      
      s.color match {
        case Black if z.color == Red =>
          val newP = createNode(p.elem, x, y, Black)
          val newZ = createNode(z.elem, z.left, z.right, Black)
          (createNode(s.elem, newP, newZ, p.color), false)
          
        case Black if y.color == Red =>
          val newP = createNode(p.elem, x, y.left, Black)
          val newS = createNode(s.elem, y.right, z, Black)
          (createNode(y.elem, newP, newS, p.color), false)
          
        case Black =>
          val newS = createNode(s.elem, y, z, Red)
          (createNode(p.elem, x, newS, Black), p.color == Black)
          
        case Red =>
          val a = y.left
          val b = y.right
          if (b.color == Red) {
            val newB = createNode(b.elem, b.left, b.right, Black)
            val newP = createNode(p.elem, x, a, Black)
            val newY = createNode(y.elem, newP, newB, Red)
            (createNode(s.elem, newY, z, Black), false)
          } else if (a.color == Red) {
            val newP = createNode(p.elem, x, a.left, Black)
            val newY = createNode(y.elem, a.right, b, Black)
            val newA = createNode(a.elem, newP, newY, Red)
            (createNode(s.elem, newA, z, Black), false)
          } else {
            val newY = createNode(y.elem, a, b, Red)
            val newP = createNode(p.elem, x, newY, Black)
            (createNode(s.elem, newP, z, Black), false)
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
          val newP = createNode(p.elem, z, x, Black)
          val newY = createNode(y.elem, y.left, y.right, Black)
          (createNode(s.elem, newY, newP, p.color), false)
          
        case Black if z.color == Red =>
          val newP = createNode(p.elem, z.right, x, Black)
          val newS = createNode(s.elem, y, z.left, Black)
          (createNode(z.elem, newS, newP, p.color), false)
          
        case Black =>
          val newS = createNode(s.elem, y, z, Red)
          (createNode(p.elem, newS, x, Black), p.color == Black)
          
        case Red =>
          val a = z.left
          val b = z.right
          if (a.color == Red) {
            val newA = createNode(a.elem, a.left, a.right, Black)
            val newP = createNode(p.elem, b, x, Black)
            val newZ = createNode(z.elem, newA, newP, Red)
            (createNode(s.elem, y, newZ, Black), false)
          } else if (b.color == Red) {
            val newP = createNode(p.elem, b.right, x, Black)
            val newZ = createNode(z.elem, a, b.left, Black)
            val newB = createNode(b.elem, newZ, newP, Red)
            (createNode(s.elem, y, newB, Black), false)
          } else {
            val newZ = createNode(z.elem, a, b, Red)
            val newP = createNode(p.elem, newZ, x, Black)
            (createNode(s.elem, y, newP, Black), false)
          }
      }
    }
  }

  def delete(e: Element[T]): RBTree[T] = {
    val data = e
    val k = data.key
    
    require(this.color == Black)

    def _delete(node: RBTree[T]): (RBTree[T], Boolean) = {
      if (k < node.key) {     // if the left is NilTree, exception will be raised.
        val (newLeft, isStale) = _delete(node.left)
        val newNode = createNode(node.elem, newLeft, node.right, node.color)

        if (isStale) resolveBlackConflict(newNode, true)
        else (newNode, false)
      } else if (k > node.key) {
        val (newRight, isStale) = _delete(node.right)
        val newNode = createNode(node.elem, node.left, newRight, node.color)

        if (isStale) resolveBlackConflict(newNode, false)
        else (newNode, false)
      } else {
        // the node to delete found.
        if (node.right.isEmpty || node.left.isEmpty) {
          val replacer = if (node.right.isEmpty) node.left else node.right
          if (node.color == Black) {
            if (replacer.color == Red) (createNode(replacer.elem, replacer.left, replacer.right, Black), false)
            else (replacer, true)
          } else (replacer, false)
        } else {    // Neither of two children is empty
          // select the replacer with the successor for the time being
          val (newRight, replacer, isStale) = getReplacer(node.right, true)
          val newNode = createNode(replacer.elem, node.left, newRight, node.color)

          if (isStale) resolveBlackConflict(newNode, false)
          else (newNode, false)
        }
      }
    }
    
    def getReplacer(node: RBTree[T], withSuccessor: Boolean): (RBTree[T], RBTree[T], Boolean) = {
      if (!withSuccessor) error("Not implemented yet")
      else {
        if (node.left.isEmpty)    // the successor is selected as the replacer.
          if (node.color == Red) (node.right, node, false)
          else if (node.right.color == Red)
            (createNode(node.right.elem, node.right.left, node.right.right, Black), node, false)
          else (node.right, node, true)
        else {
          val (newLeft, replacer, isStale) = getReplacer(node.left, withSuccessor)
          val newNode = createNode(node.elem, newLeft, node.right, node.color)
          
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
    if (root.color == Red) createNode(root.elem, root.left, root.right, Black)
    else root
  }
}


object RBTree {
  def build[T : Ordering](xs: List[Element[T]]): RBTree[T] = {
    val nil = new NilTree[T]
    if (xs.length == 0) nil
    else {
      val tree: RBTree[T] = new NormalTree[T](xs.head, nil, nil, Black, nil)
      
      (tree /: xs.tail) { _.insert(_) }
    }
  }
}