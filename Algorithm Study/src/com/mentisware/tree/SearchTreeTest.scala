package com.mentisware.tree

import com.mentisware.test.UnitSpec
import com.mentisware.sort.Sorter

case class IntElem(key: Int) extends Element[Int]

trait SearchTreeBehavior { this: UnitSpec =>
  val sort = Sorter.quickSort[Int] _
  
  def buildTreeInOrder(xs: List[Int])(buildTree: Seq[Element[Int]] => SearchTree[Int]) {
    val es = xs.map(IntElem)
    val tree = buildTree(es)
    val sortedKeys = sort(xs)

    it should "pass the validation just after tree build" in {
      tree.validate()
    }
    
    it should "return the correct min element" in {
      tree.minimum match {
        case Some(e) => (e.key) should equal (sortedKeys.head)
        case _ => assert(false, "minimum not found")
      }
    }
    
    it should "return the correct max element" in {
      tree.maximum match {
        case Some(e) => (e.key) should equal (sortedKeys.last)
        case _ => assert(false, "maximum not found")
      }
    }
    
    it should "search the element with a given key" in {
      (xs.head) should equal (tree(xs.head).key)
    }
    
    it should "return a sorted key list" in {
      val zippedPair = sortedKeys zip tree.keyList
      zippedPair foreach { x =>
        (x._1) should equal (x._2)
      }
    }
  }
  
  def performComplexOperations(xs: List[Int])(buildTree: Seq[Element[Int]] => SearchTree[Int]) {
    val es = xs.toVector.map(IntElem)
    val len = es.length
    val ys = for (i <- (1 until len by 2).toVector) yield xs(i)
    val zs = ys ++ (for (i <- (0 until len/2 by 2).toVector) yield xs(i))
    var tree = buildTree(es)

    
    it should "delete a half of elements correctly" in {
      for (i <- 0 until len by 2) {
        tree = tree.delete(es(i))
      }
      tree.validate()

      val zippedPair = sort(ys.toList) zip tree.keyList
      zippedPair foreach { x =>
        (x._1) should equal (x._2)
      }
    }

    it should "add a half of the deleted elements in reverse order" in {
      for (i <- (0 until len/2 by 2).reverse) {
        tree = tree.insert(es(i))
      }
      tree.validate()

      val zippedPair = sort(zs.toList) zip tree.keyList
      zippedPair foreach { x =>
        (x._1) should equal (x._2)
      }
    }
    
    it should "return an empty tree after deleting all elements" in {
      zs foreach { k => tree = tree.delete(IntElem(k)) }
      assert(tree.isEmpty(), "Tree is not empty")
    }
  }
}