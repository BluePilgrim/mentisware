package com.mentisware.tree.rbtree

import com.mentisware.tree._

import com.mentisware.test.UnitSpec
import com.mentisware.sort.Orderer

case class IntElem (key: Int) extends Element[Int]


trait RBTreeBehavior { this: UnitSpec =>
  def performPrimitiveOperations(xs: List[Int], tree: RBTree[Int]) {
    val es = xs.map(IntElem(_))
    
    
    it should "return the minimum number" in {
      (tree.minimum) should equal (Some(IntElem(Orderer.selectSmallest(xs, 1))))
    }
    
    it should "return the maximum number" in {
      (tree.maximum) should equal (Some(IntElem(Orderer.selectSmallest(xs, xs.length))))
    }
    
    it should "pass the RB property validation after insert" in {
      tree.validate()
    }

    it should "pass the RB property validation after delete" in {
      tree.delete(es.head).delete(es.tail.head).validate()
    }
    
    it should "return NilTree after deletion of all elements" in {
      val result = (tree /: es) { (t, e) =>
//        println("Delete " + e.key + " from\n" + t)
        t.validate()
        t.delete(e)
      }
 
      (result.isEmpty()) should equal (true)
    }
  }
  
  def performComplexOperations(xs: List[Int], tree: RBTree[Int]) {
    val es = xs.map(IntElem(_))
    val len = xs.length

    var t = tree

    it should "delete a half of elements" in {
      for (i <- 0 until len by 2) t = t.delete(es(i))
      t.validate()
    }

    it should "add a half of the deleted elements in reverse order" in {
      for (i <- (0 until len/2 by 2).reverse) t = t.insert(es(i))
      t.validate()
    }
  }
  
  def processOrderingStatistics(xs: List[Int], tree: RBTree[Int]) {
    val es = xs.map(IntElem(_))
    val len = xs.length
    val rnd = new scala.util.Random(System.currentTimeMillis())
    
    it should "select random orders correctly" in {
      for (i <- 1 to len/2) {
        val order = rnd.nextInt(len) + 1    // nextInt exclude len
        (tree.selectSmallest(order).key) should equal (Orderer.selectSmallest(xs, order))
      }
    }
    
    it should "return correct rank information of random data in test set" in {
      // we should take care of multiple identical keys
      for (i <- 1 to len/2) {
        val order = rnd.nextInt(len) + 1
        val node = tree.selectSmallest(order)
//        println("order = " + order + " with node key = " + node.key)
//        (node.rankIn(tree)) should equal (order)
        (tree.selectSmallest(node.rankIn(tree)).key) should equal (node.key)
      }
    }
  } 
}


class RBTreeTest extends UnitSpec with RBTreeBehavior {
  def determinedSet = {

    val xs = List(3, 11, 10, -1, -45, 2, -4, 7, -5, 9, 1, 100, 2, 4, -100, 98, 46)
    
    (xs,
    RBTree.build[Int](xs.map(IntElem(_))),
    OrderingTree.build[Int](xs.map(IntElem(_)))
    )
  }

  def randomSet(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    val xs = List.fill(size)(rnd.nextInt(100000))
    
    (xs,
    RBTree.build[Int](xs.map(IntElem(_))),
    OrderingTree.build[Int](xs.map(IntElem(_)))
    )
  }

  val detSet = determinedSet
  val rndSet = randomSet(10000)

  "Determined Set with Red Black Tree" should behave like performPrimitiveOperations(detSet._1, detSet._2)
  it should behave like performComplexOperations(detSet._1, detSet._2)
  it should behave like processOrderingStatistics(detSet._1, detSet._2)

  "Random Set with Red Black Tree" should behave like performPrimitiveOperations(rndSet._1, rndSet._2)
  it should behave like performComplexOperations(rndSet._1, rndSet._2)
  it should behave like processOrderingStatistics(rndSet._1, rndSet._2)
  

  "Determined Set with Ordering Tree" should behave like performPrimitiveOperations(detSet._1, detSet._3)
  it should behave like performComplexOperations(detSet._1, detSet._3)
  it should behave like processOrderingStatistics(detSet._1, detSet._3)

  "Random Set with Ordering Tree" should behave like performPrimitiveOperations(rndSet._1, rndSet._3)
  it should behave like performComplexOperations(rndSet._1, rndSet._3)
  it should behave like processOrderingStatistics(rndSet._1, rndSet._3)

}