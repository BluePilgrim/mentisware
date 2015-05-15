package com.mentisware.graph

import com.mentisware.test.UnitSpec

class AdjMatrixGraphTest extends UnitSpec with GraphBehavior with GraphTestSet {
  graphs foreach { set =>
//    val es: List[(Int, Int, Double)] = set._3.map(x => (x._1, x._2, x._3.toDouble))
    val graph = AdjMatrixGraph(set._1, set._2, set._3: _*)
    
    "For adjacent matrix graph: " + set should behave like checkProperty(graph)(set._2, set._3.length, set._1)
    it should behave like traverseInBFS(graph)(graph.vertices(set._2-1), graph.vertices(1))
    it should behave like traverseInDFS(graph)(graph.vertices(set._2-1), graph.vertices(1))
    it should behave like performTopologicalSort(graph)
    it should behave like calculateSCC(graph)
    it should behave like calculateMST(graph)
    it should behave like calculateShortestPathFrom(graph)(graph.vertices(0))
    it should behave like calculateAllPairsShortestPath(graph)
    it should behave like performJohnsonsAlgorithm(graph)
  }
}