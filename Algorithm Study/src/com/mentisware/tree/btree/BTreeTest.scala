package com.mentisware.tree.btree

import com.mentisware.tree.Element

import com.mentisware.test.UnitSpec
import com.mentisware.sort.Orderer
import com.mentisware.tree.rbtree.RBTree

case class IntElem(key: Int) extends Element[Int]

trait BTreeBehavior { this: UnitSpec =>
  def performPrimitiveOperations(xs: List[Int], minDegree: Int) {
    val es = xs.map(IntElem(_))
    val tree = BTree.build(es, minDegree)
    tree.validateBTree()    
    
    it should "return the minimum number" in {
      (tree.minimum) should equal (Some(IntElem(Orderer.selectSmallest(xs, 1))))
    }
    
    it should "return the maximum number" in {
      (tree.maximum) should equal (Some(IntElem(Orderer.selectSmallest(xs, xs.length))))
    }
    
    it should "return an empty tree after deletion of all elements" in {
      val result = (tree /: es) { _.delete(_) }
//      val result = (tree /: es) { (t, e) =>
//        val tr = t.delete(e)
//        println("after delete " + e)
//        tr.validateBTree()
//        tr}
      
//      println("es = " + es)
//      println("key list = " + result.keyList)
      (result.isEmpty()) should equal (true)
    }
  }
  
  def performComplexOperations(xs: List[Int], minDegree: Int) {
    val es = xs.map(IntElem(_))
    val len = xs.length
    val tree = BTree.build(es, minDegree)
    tree.validateBTree()
    
    var t = tree

    it should "delete a half of elements" in {
      for (i <- 0 until len by 2) {
        t = t.delete(es(i))
//        println("delete " + es(i))
        t.validateBTree()
      }
    }

    it should "add a half of the deleted elements in reverse order" in {
      for (i <- (0 until len/2 by 2).reverse) t = t.insert(es(i))
    }
  }
}

trait BTreeTestSet {
  def degrees = List(2, 5, 8, 10, 50)
  
  def determinedSet = {
    List(64, 52, 73, 89, 70, 25, 69, 95, 7, 27, 8, 71, 67, 81, 96, 32, 9, 55, 10, 96, 97, 41, 99, 20, 20, 91, 52, 17, 9, 31)
  }

  def randomSet(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    List.fill(size)(rnd.nextInt(100000))
  }
}

class BTreeTest extends UnitSpec with BTreeBehavior with BTreeTestSet {
  val detSet = determinedSet
  val rndSet = randomSet(10000)
  
  degrees.map { t =>
    "Determined Set with B Tree(min degree = " + t +")" should behave like performPrimitiveOperations(detSet, t)
    it should behave like performComplexOperations(detSet, t)
  
    "Random Set with B Tree(min degree = " + t +")" should behave like performPrimitiveOperations(rndSet, t)
    it should behave like performComplexOperations(rndSet, t)
  }
}