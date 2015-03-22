package com.mentisware.tree.btree

import com.mentisware.test.UnitSpec
import com.mentisware.tree.Element
import com.mentisware.tree.SearchTreeBehavior

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

class BTreeTest extends UnitSpec with SearchTreeBehavior with BTreeTestSet {
  val detSet = determinedSet
  val rndSet = randomSet(10000)
  
  degrees.map { t =>
    "Determined Set with B Tree(min degree = " + t +")" should behave like buildTreeInOrder(detSet)(BTree.build[Int](t))
    it should behave like performComplexOperations(detSet)(BTree.build(t))
  
    "Random Set with B Tree(min degree = " + t +")" should behave like buildTreeInOrder(rndSet)(BTree.build(t))
    it should behave like performComplexOperations(rndSet)(BTree.build(t))
  }
}