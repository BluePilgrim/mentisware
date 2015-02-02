package com.mentisware.huffman

/**
 * @author David Yang
 */

sealed abstract class PrefixTree { val freq: Int }

case class InternalNode(freq: Int, left: PrefixTree, right: PrefixTree) extends PrefixTree
case class LeafNode(freq: Int, ch: Char) extends PrefixTree
case class NullNode(freq: Int = 0) extends PrefixTree

object PrefixTree {
  def build(freqTable: Map[Char, Int]): PrefixTree = {
    import com.mentisware.sort.Heap
    
    // make leaf nodes first and insert the node into min heap
    val leafNodes = freqTable.toList.map(x => LeafNode(x._2, x._1))
    val minHeap = new Heap[PrefixTree](leafNodes, false)
    
    for (i <- 0 until leafNodes.size - 1) {
      val x = minHeap.extractHead match {
        case Some(n) => n
        case _ => NullNode()
      }
      val y = minHeap.extractHead match {
        case Some(n) => n
        case _ => NullNode()
      }
      
      val z = InternalNode(x.freq + y.freq, x, y)
//      println("z = " + z)
      minHeap.add(z)
    }
    
    assert(minHeap.getLength == 1)
    
    minHeap.getHead() match {
      case Some(x) => x
      case _ => NullNode()
    }
  }
  
  def printHuffmanCode(pTree: PrefixTree) {
    def printEncoding(node: PrefixTree, code: Int, len: Int): Unit = node match {
      case x: LeafNode => println(x.ch + " = " + f"$code%x" + " (" + len + ")")
      case i: InternalNode =>
        printEncoding(i.left, code << 1, len + 1)
        printEncoding(i.right, (code << 1) | 1, len + 1)
      case _ => assert(false)
    }
    
    printEncoding(pTree, 0, 0)
  }
  
  implicit val ord = new Ordering[PrefixTree] {
    def compare(x: PrefixTree, y: PrefixTree) = x.freq - y.freq
  }
}