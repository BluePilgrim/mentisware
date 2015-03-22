package com.mentisware.tree.rbtree

import com.mentisware.tree._
import com.mentisware.sort.Orderer
import com.mentisware.test.UnitSpec

case class IntElem (key: Int) extends Element[Int]


trait RBTreeBehavior extends SearchTreeBehavior { this: UnitSpec =>
  def processOrderingStatistics(xs: List[Int])(buildTree: Seq[Element[Int]] => RBTree[Int]) {
    val es = xs.map(IntElem(_))
    val len = xs.length
    val tree = buildTree(es)
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

trait RBTestSet {
  def determinedSet = List(3, 11, 10, -1, -45, 2, -4, 7, -5, 9, 1, 100, 2, 4, -100, 98, 46)

  def randomSet(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    List.fill(size)(rnd.nextInt(100000))
  }  
}


class RBTreeTest extends UnitSpec with RBTreeBehavior with RBTestSet {
  val detSet = determinedSet
  val rndSet = randomSet(10000)

  "Determined Set with Red Black Tree" should behave like buildTreeInOrder(detSet)(RBTree.build[Int])
  it should behave like performComplexOperations(detSet)(RBTree.build[Int])
  it should behave like processOrderingStatistics(detSet)(RBTree.build[Int])

  "Random Set with Red Black Tree" should behave like buildTreeInOrder(rndSet)(RBTree.build[Int])
  it should behave like performComplexOperations(rndSet)(RBTree.build[Int])
  it should behave like processOrderingStatistics(rndSet)(RBTree.build[Int])
  

  "Determined Set with Ordering Tree" should behave like buildTreeInOrder(detSet)(OrderingTree.build[Int])
  it should behave like performComplexOperations(detSet)(OrderingTree.build[Int])
  it should behave like processOrderingStatistics(detSet)(OrderingTree.build[Int])

  "Random Set with Ordering Tree" should behave like buildTreeInOrder(rndSet)(OrderingTree.build[Int])
  it should behave like performComplexOperations(rndSet)(OrderingTree.build[Int])
  it should behave like processOrderingStatistics(rndSet)(OrderingTree.build[Int])
}