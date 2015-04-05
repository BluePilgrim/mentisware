package com.mentisware.tree

import com.mentisware.test.UnitSpec
import com.mentisware.sort.Sorter

case class IntElem(key: Int) extends Element[Int]
case class IntLoad(key: Int, data: Int) extends Element[Int] {
  override def toString = "(" + key + ", " + data + ")"
}

trait SearchTreeBehavior { this: UnitSpec =>
  val sort = Sorter.quickSort[Int] _
  
  def buildTreeInOrder(xs: Seq[Int])(buildTree: Seq[Element[Int]] => SearchTree[Int]) {
    val es = xs.map(IntElem)
    val tree = buildTree(es)
    val sortedKeys = sort(xs.toList)

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
//      println(zippedPair)
      zippedPair foreach { x =>
        (x._1) should equal (x._2)
      }
    }
  }
  
  def performComplexOperations(xs: Seq[Int])(buildTree: Seq[Element[Int]] => SearchTree[Int]) {
    val es = xs.toVector.map(IntElem)
    val len = es.length
    val ys = for (i <- (1 until len by 2).toVector) yield xs(i)
    val zs = ys ++ (for (i <- (0 until len/2 by 2).toVector) yield xs(i))
    var tree = buildTree(es)

//    println("key list =\n" + tree.keyList)
    it should "delete a half of elements correctly" in {
//      println("delete key seq =")
      for (i <- 0 until len by 2) {
//        print(" " + es(i))
        tree = tree.delete(es(i))
      }
      tree.validate()

      val zippedPair = sort(ys.toList) zip tree.keyList
//      println("\n" + zippedPair)
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
      zs foreach { k => tree = tree.delete(IntElem(k))}
      assert(tree.isEmpty(), "Tree is not empty")
    }
  }
  
  def dealWithDuplicateKey(xs: Seq[Int])(buildTree: Seq[Element[Int]] => SearchTree[Int]) {
    val len = xs.length
    var tree = buildTree(((xs) zip (0 until len)).map(x => IntLoad(x._1,x._2)))

    val k = xs(0)
    for (i <- 0 to 30) {
      tree = tree.insert(IntLoad(k, i))
    }
    val e = tree(k)
    val es = tree.search(k)

//    println("for k=" + k + "\nes = " + es)
    
    it should "find all elements for given key" in {
      (es.length) should be >= 11
      (es(0).key) should equal (es(1).key)
      (es(0)) should not equal (es(5))
      (es(0).key) should equal (e.key)
    }
    
    it should "delete the precise element" in {
      val e1 = es(1)
      val es1 = (tree.delete(e1)).search(k)
//      println ("es1 = " + es1)
      
      (es1.length) should equal (es.length - 1)
      (e1) should not equal (es1(1))
    }
  }
}