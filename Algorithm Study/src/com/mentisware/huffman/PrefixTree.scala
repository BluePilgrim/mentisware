package com.mentisware.huffman

/**
 * @author David Yang
 */

sealed abstract class PrefixTree {
  val freq: Int

  def decodeChar(encData: Int): (Char, Int) = {  // assume the encode data start from 31th bit (MSB order)
    def traverseTree(node: PrefixTree, bitPos: Int, depth: Int): (Char, Int) = node match {
      case x: LeafNode => (x.ch, depth)
      case i: InternalNode =>
        traverseTree(
          if ((encData >>> bitPos & 0x1) == 0) i.left else i.right,
          bitPos - 1,
          depth + 1
        )
      case _ => assert(false); (' ', 0)
    }
    
    traverseTree(this, 31, 0)
  }
  
  def getCodeTable: Map[Char, (Int, Int)] = {
    val codeTable = scala.collection.mutable.Map[Char, (Int, Int)]()
    
    def calculateEncoding(node: PrefixTree, code: Int, len: Int): Unit = node match {
      case x: LeafNode => codeTable(x.ch) = (code, len)
      case i: InternalNode =>
        calculateEncoding(i.left, code << 1, len + 1)
        calculateEncoding(i.right, (code << 1) | 1, len + 1)
      case _ => assert(false)
    }
    
    calculateEncoding(this, 0, 0)
    codeTable.toMap
  }
  
  override def toString = {
    val codeTable = getCodeTable
    codeTable.view map {
      case (ch, (code, codeLen)) => ch + " = " + f"$code%x" + " (" + codeLen + ")"
    } mkString("", "\n", "")
  }

/*
  def printHuffmanCode {
    def printEncoding(node: PrefixTree, code: Int, len: Int): Unit = node match {
      case x: LeafNode => println(x.ch + " = " + f"$code%x" + " (" + len + ")")
      case i: InternalNode =>
        printEncoding(i.left, code << 1, len + 1)
        printEncoding(i.right, (code << 1) | 1, len + 1)
      case _ => assert(false)
    }
    
    printEncoding(this, 0, 0)
  }
*/
}

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
  
  def build(src: scala.io.Source): PrefixTree = {
    val freqTable = scala.collection.mutable.Map[Char, Int]().withDefault(x => 0)
    
    src foreach (freqTable(_) += 1)
    
    build(freqTable.toMap)
  }
  
  implicit val ord = new Ordering[PrefixTree] {
    def compare(x: PrefixTree, y: PrefixTree) = x.freq - y.freq
  }
}