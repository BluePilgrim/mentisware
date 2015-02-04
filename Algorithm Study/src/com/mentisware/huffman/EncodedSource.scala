package com.mentisware.huffman

import scala.io.Source


class EncodedSource(val src: Source) {
  val prefixTree = PrefixTree.build(src)
  val huffmanCode = prefixTree.getCodeTable
  
    
}