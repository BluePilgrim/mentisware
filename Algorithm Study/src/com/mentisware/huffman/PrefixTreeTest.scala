package com.mentisware.huffman

import com.mentisware.test.UnitSpec

trait TestSet {
  def set1 = Map('a' -> 10, 'b' -> 4, 'c' -> 100, 'd' -> 40, 'e' -> 20)
  def set2 = Map('f' -> 5, 'e' -> 9, 'c' -> 12, 'b' -> 13, 'd' -> 16, 'a' -> 45)
  
  def inputFile = "/Users/David Yang/Downloads/pg1661.txt"
}

class PrefixTreeTest extends UnitSpec with TestSet {
  "Prefix Tree" should "run the Huffman encoding for set 1" in {
    val freqTable = set2
    val prefixTree = PrefixTree.build(freqTable)
    PrefixTree.printHuffmanCode(prefixTree)
  }
  
  it should "read a text file and build a prefix tree" in {
    val src = scala.io.Source.fromFile(inputFile)
    val freqTable = scala.collection.mutable.Map[Char, Int]().withDefault(x => 0)
    
    src foreach (freqTable(_) += 1)
    
    val prefixTree = PrefixTree.build(freqTable.toMap)
    PrefixTree.printHuffmanCode(prefixTree)
  }
}