package com.mentisware.graph

import com.mentisware.test.UnitSpec

trait GraphTestSet {
  def set1 = ( false, 8,
    List((0, 4), (0, 1), (1, 5), (2, 3), (2, 5), (2, 6), (3, 6), (3, 7), (5, 6), (6, 7))
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
    val t = g.bfsTree(s)
    val p = t.path(s, d)
    
    it should "traverse a graph in BFS" in {
      (t.parent(s)) should equal (null)
    }
    
    it should "return a BFS path correctly" in {
      def checkParent(path: List[g.V], p: g.V) {
        if (path != Nil) {
          assert(t.parent(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      checkParent(p.tail, s)
    }
  }
}