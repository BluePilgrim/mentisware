package com.mentisware.graph

import com.mentisware.test.UnitSpec

trait GraphTestSet {
  def graphs = List(
      (false, 8, List((0, 4), (0, 1), (1, 5), (2, 3), (2, 5), (2, 6), (3, 6), (3, 7), (5, 6), (6, 7))),
      (true, 8, List((0, 4), (1, 0), (4, 1), (1, 5), (5, 4), (2, 1), (2, 5), (6, 2), (6, 5), (3, 6), (7, 6), (3, 7), (7, 3)))
  )
}

trait GraphBehavior { this: UnitSpec =>
  def checkProperty(g: Graph)(nVertices: Int, nEdges: Int, isDirected: Boolean) {
    it should "return basic properties correctly" in {
      (g.isDirected) should equal (isDirected)
      (g.nVertices) should equal (nVertices)
      if (isDirected)
        (g.nEdges) should equal (nEdges)
      else
        (g.nEdges) should equal (2 * nEdges)
    }
  }
  
  def traverseInBFS(g: Graph)(s: g.V, d: g.V) {
    val (pGraph, stamp) = g.bfsGraph(s)
    val p = pGraph.path(s, d)
    
    it should "traverse a graph in BFS" in {
      (pGraph.pred(s)) should equal (null)
      g.vertices foreach { v =>
        println(v + " (" + stamp.discover(v) + ", " + stamp.finish(v) + ")" )
      }
    }
    
    it should "return a BFS path correctly" in {
      def checkParent(path: List[g.V], p: g.V) {
        if (path != Nil) {
          assert(pGraph.pred(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      if (!p.isEmpty)
        checkParent(p.tail, s)
      else
        assert(!(stamp.discover(s) < stamp.discover(d) && stamp.finish(s) > stamp.finish(d) ||
            stamp.discover(s) > stamp.discover(d) && stamp.finish(s) < stamp.finish(d)))
            
      println("path from " + s + " to " + d + " = " + p)
    }
  }

  def traverseInDFS(g: Graph)(s: g.V, d: g.V) {
    val (pGraph, stamp) = g.dfsGraph(s)
    val p = pGraph.path(s, d)
    
    it should "traverse a graph in DFS" in {
      (pGraph.pred(s)) should equal (null)
      g.vertices foreach { v =>
        println(v + " (" + stamp.discover(v) + ", " + stamp.finish(v) + ")" )
      }
    }
    
    it should "return a DFS path correctly" in {
      def checkParent(path: List[g.V], p: g.V) {
        if (path != Nil) {
          assert(pGraph.pred(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      if (!p.isEmpty) checkParent(p.tail, s)
      println("path from " + s + " to " + d + " = " + p)
    }
  }
}