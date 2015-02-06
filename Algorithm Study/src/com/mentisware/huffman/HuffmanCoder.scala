package com.mentisware.huffman

import scala.io.Source


object HuffmanCoder {
  def encode(src: Source): (PrefixTree, Array[Int], Long) = {
    val srcToEncode = src.reset
    val prefixTree = PrefixTree.build(src)
    val huffmanCode = prefixTree.getCodeTable
  
    val stream = new scala.collection.mutable.ArrayBuilder.ofInt
    var buf = 0
    var bitPos = 32
    var bitCount = 0L

    // assume MSB order
    srcToEncode foreach { ch =>
      val (code, len) = huffmanCode(ch)
      
      assert(len <= 32)

      if (bitPos - len >= 0) {
        bitPos -= len
        buf |= code << bitPos
      } else {
        // put the upper "bitPos" bits in the buffer
        val overflowSize = len - bitPos
        buf |= code >>> overflowSize
        
        // flush the buffer
        stream += buf
        bitCount += 32

        // put the lower "len-bitPos" bits in the buffer
        bitPos = 32 - (overflowSize)
        buf = code << bitPos
      }
    }

    // take care of dangling data at the end
    // 0 <= bitPos <= 31
    assert(bitPos >= 0 && bitPos <= 31)
    stream += buf
    bitCount += (32 - bitPos)
    
    (prefixTree, stream.result, bitCount)
  } 
  
  def decode(prefixTree: PrefixTree, encSrc: Array[Int], bitCount: Long): Source = {
    val charStream = new scala.collection.mutable.ArrayBuilder.ofChar
    
    var i = 0
    var bitPos = 32

    var encData = encSrc(0)
    var lookaheadData = 0
    var remainingLen = bitCount
      
    while (i < encSrc.length - 1) {
      lookaheadData = encSrc(i + 1)

      val (ch, len) = prefixTree.decodeChar {
        if (bitPos < 32) encData << (32 - bitPos) | lookaheadData >>> bitPos else encData
      }
//      println("ch = " + ch + " i = " + i + " bitPos = " + bitPos )
      charStream += ch
      remainingLen -= len
      bitPos -= len

      if (bitPos <= 0) {
        i += 1
        bitPos += 32
        encData = lookaheadData
      }
    }
    
    // now i >= encSrc.length - 1
    while (bitPos > 0 && remainingLen > 0) {
      val (ch, len) = prefixTree.decodeChar(encData << (32 - bitPos))
//      println("ch = " + ch)
      
      charStream += ch
      remainingLen -= len
      bitPos -= len
      
      assert(bitPos >= 0)
    }
    
    assert(remainingLen == 0)
    
    Source.fromChars(charStream.result)
  }
}