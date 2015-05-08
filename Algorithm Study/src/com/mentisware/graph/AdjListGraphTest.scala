package com.mentisware.graph

import com.mentisware.test.UnitSpec

class AdjListGraphTest extends UnitSpec with GraphBehavior with GraphTestSet {
  val es: List[(Int, Int, Double)] = set1._3.map(x => (x._1, x._2, 1.0))
  val graph = AdjListGraph(set1._1, set1._2, es: _*)
  
  "Undirected Graph(Fig 22.3)" should behave like checkProperty(graph)(set1._2, set1._3.length, set1._1)
  it should behave like traverseInBFS(graph)(graph.vertex(1), graph.vertex(7))
}