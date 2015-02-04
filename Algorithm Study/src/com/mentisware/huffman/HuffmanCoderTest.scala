package com.mentisware.huffman

import com.mentisware.test.UnitSpec
import scala.io.Source

trait HuffmanCoderBehaviors { this: UnitSpec =>
  def unitOperation(src: => Source) {
    val (prefixTree, stream, bitCount) = HuffmanCoder.encode(src)
      
    it should "encode a source and generate prefix tree correctly" in {
      println(prefixTree)
      println("original size = " + src.length * 8 + " : encoded size = " + bitCount)
    }

    it should "decode an encoded source correctly" in {
      val decodedSrc = HuffmanCoder.decode(prefixTree, stream, bitCount)
      (decodedSrc.length) should equal (src.length)
      
    }
  }
  
  def transitiveOperation(src: => Source) {
    it should "return the original data after decode after encode" in {
      val orgPrefixTree = PrefixTree.build(src)
      val (prefixTree, stream, bitCount) = HuffmanCoder.encode(src)
      val decodedSrc = HuffmanCoder.decode(prefixTree, stream, bitCount)
      
      assert((true /: (src zip decodedSrc))((x, y) => x && y._1 == y._2))
    }
    
    it should "return the same encoded data after encode after decode after encode" in {
      val encodedResult1 = HuffmanCoder.encode(src)
      val encodedResult2 = HuffmanCoder.encode(
          HuffmanCoder.decode(encodedResult1._1, encodedResult1._2, encodedResult1._3))
          
      (encodedResult1._3) should equal (encodedResult2._3)
    }
  }
}

class HuffmanCoderTest extends UnitSpec with HuffmanCoderBehaviors {
  def testSet1 = Source.fromString("This is a test.")
  def testSet2 = Source.fromFile("/Users/David Yang/Downloads/pg1661.txt")

  "Huffman Coder (from test string)" should behave like unitOperation(testSet1)
  it should behave like transitiveOperation(testSet1)
  
  "Huffman Coder (from a file)" should behave like unitOperation(testSet2)
  it should behave like transitiveOperation(testSet2)
}