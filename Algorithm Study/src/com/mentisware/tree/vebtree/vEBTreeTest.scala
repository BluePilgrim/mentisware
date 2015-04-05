package com.mentisware.tree.vebtree

import com.mentisware.test.UnitSpec
import com.mentisware.tree.Element
import com.mentisware.tree.SearchTreeBehavior

trait vEBTreeTestSet {
  def determinedSet = (
    List(64, 52, 73, 89, 70, 25, 69, 95, 7, 27, 8, 71, 67, 81, 96, 32, 9, 55, 10, 96, 97, 41, 99, 20, 20, 91, 52, 17, 9, 31),
    100
//    List(1, 4, 2, 2, 1, 7, 6, 6, 3), 8
  )

  def randomSet(size: Int = 1000) = {
    val univSize = 200000
    val rnd = new scala.util.Random(System.currentTimeMillis())
    (List.fill(size)(rnd.nextInt(univSize-1)), univSize)
  }
}

class vEBTreeTest extends UnitSpec with SearchTreeBehavior with vEBTreeTestSet {
  val detSet = determinedSet
  val rndSet = randomSet(100000)
  
  "Determined Set with vEB Tree(univ size = " + detSet._2 +")" should behave like buildTreeInOrder(detSet._1)(vEBTree.build(detSet._2))
  it should behave like performComplexOperations(detSet._1)(vEBTree.build(detSet._2))
  it should behave like dealWithDuplicateKey(detSet._1)(vEBTree.build(detSet._2))

  "Random Set with vEB Tree(univ size = " + rndSet._2 +")" should behave like buildTreeInOrder(rndSet._1)(vEBTree.build(rndSet._2))
  it should behave like performComplexOperations(rndSet._1)(vEBTree.build(rndSet._2))
  it should behave like dealWithDuplicateKey(rndSet._1)(vEBTree.build(rndSet._2))
}