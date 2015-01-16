package com.mentisware.ds

import com.mentisware.test.UnitSpec
import com.mentisware.sort._

case class IntNode (key: Int) extends Node[Int]

trait RBTreeBehavior { this: UnitSpec =>
  def performPrimitiveOperations(testSet: (List[Int], RBTree[Int, IntNode])) {
    val (xs, tree) = testSet
    val es = xs.map(IntNode(_))
    
//    it should "insert test data in correct order" in {
//      println(tree)
//    }
    
    it should "return the minimum number" in {
      (tree.minimum) should equal (Some(IntNode(Orderer.selectSmallest(xs, 1))))
    }
    
    it should "return the maximum number" in {
      (tree.maximum) should equal (Some(IntNode(Orderer.selectSmallest(xs, xs.length))))
    }
    
    it should "pass the RB property validation after insert" in {
      tree.validateRBProperty()
    }

    it should "pass the RB property validation after delete" in {
      tree.delete(es.head).delete(es.tail.head).validateRBProperty()
    }
    
    it should "return NilTree after deletion of all elements" in {
      val result = (tree /: es) { (t, e) =>
//        println("Delete " + e.key + " from\n" + t)
        t.validateRBProperty()
        t.delete(e)
      }
 
      (result) should equal (NilTree)
    }
  }
  
  def performComplexOperations(testSet: (List[Int], RBTree[Int, IntNode])) {

    val (xs, tree) = testSet
    val es = xs.map(IntNode(_))
    val len = xs.length

    var t = tree

    it should "delete a half of elements" in {
      for (i <- 0 until len by 2) t = t.delete(es(i))
      t.validateRBProperty()
    }

    it should "add a half of the deleted elements in reverse order" in {
      for (i <- (0 until len/2 by 2).reverse) t = t.insert(es(i))
      t.validateRBProperty()
    }
  }
}


class RBTreeTest extends UnitSpec with RBTreeBehavior {
  def testSet1 = {
    val xs = List(3, 11, 10, -1, -45, 2, -4, 7, -5, 9, 1, 100, 2, 4, -100, 98, 46)
    val tree = RBTree.build[Int, IntNode](xs.map(IntNode(_)))
    (xs, tree)    
  }

  def testSet2 = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs = List.fill(1000000)(rnd.nextInt(1000000))
    val tree = RBTree.build[Int, IntNode](xs.map(IntNode(_)))
    (xs, tree)
  }
  
  "Test Set 1" should behave like performPrimitiveOperations(testSet1)
  it should behave like performComplexOperations(testSet1)
  
//  "Random Set" should behave like performPrimitiveOperations(testSet2)
//  it should behave like performComplexOperations(testSet2)
}