package com.mentisware.graph

import com.mentisware.test.UnitSpec

class AdjListGraphTest extends UnitSpec with GraphBehavior with GraphTestSet {
  graphs foreach { set =>
    val es: List[(Int, Int, Double)] = set._3.map(x => (x._1, x._2, 1.0))
    val graph = AdjListGraph(set._1, set._2, es: _*)
    
    "For graph: " + set should behave like checkProperty(graph)(set._2, set._3.length, set._1)
    it should behave like traverseInBFS(graph)(graph.vertex(7), graph.vertex(1))
    it should behave like traverseInDFS(graph)(graph.vertex(7), graph.vertex(1))
  }
}