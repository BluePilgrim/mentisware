package com.mentisware.ds

import com.mentisware.test.UnitSpec
import com.mentisware.sort._

case class IntNode (key: Int) extends Node[Int]

trait RBTreeBehavior { this: UnitSpec =>
  def correctPrimitiveOperations(testSet: (List[Int], RBTree[Int, IntNode])) {
    var tree = testSet._2
    val xs = testSet._1
      
    it should "insert data in correct order" in {
      xs foreach { x =>
        tree = tree.insert(IntNode(x))
      }
      
      println(tree)
    }
    
    it should "return the minimum number" in {
      (tree.minimum) should equal (Some(IntNode(Orderer.selectSmallest(xs, 1))))
    }
    
    it should "return the maximum number" in {
      (tree.maximum) should equal (Some(IntNode(Orderer.selectSmallest(xs, xs.length))))
    }
    
    it should "delete minimum correctly" in {
      val treeMin = tree.minimum match {
        case Some(x) => x
        case _ => assert(false)
      }
      (tree.delete(treeMin).minimum) should not equal (treeMin)
    }
    
    it should "delete maximum correctly" in {
      val treeMax = tree.maximum match {
        case Some(x) => x
        case _ => assert(false)
      }
      (tree.delete(treeMax).maximum) should not equal (treeMax)
    }
  }
}


class RBTreeTest extends UnitSpec with RBTreeBehavior {
  def testSet1 = {
    val xs = List(3, 11, 10, -1, 45, 2, -4, 7, -5, 9)
    val tree: RBTree[Int, IntNode] = NormalTree[Int, IntNode](IntNode(0), NilTree, NilTree, Black)
    (xs, tree)
  }
  
  "A Red Black Tree" should behave like correctPrimitiveOperations(testSet1)
      
}