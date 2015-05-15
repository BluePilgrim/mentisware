package com.mentisware.graph

// assume that a vertex's id is set to the index of the adjacency list

class AdjListGraph(
    directed: Boolean,
    val vertices: Vector[Vertex],
    val adjList: Vector[List[(Vertex, Double)]]) extends Graph {
  require(vertices.length == adjList.length)
  def isDirected = directed
  def nVertices = vertices.length
  
  def edges = (vertices zip adjList).flatMap { x =>
    val (s, l) = x
    l.map(y => Edge(s, y._1, y._2))
  }
  def nEdges = edges.length
  def edge(src: Vertex, dst: Vertex) = {
    adjList(src).find(_._1 == dst) match {
      case Some((v, w)) => Some(Edge(src, v, w))
      case _ => None
    }
  }
  def weight(src: Vertex, dst: Vertex) = edge(src, dst) match {
    case Some(Edge(_, _, w)) => w
    case _ => if (src == dst) 0.0 else Double.PositiveInfinity
  }
  
  def adjVertices(s: Vertex) = adjList(s)
    
  def transpose() = {
    if (isDirected) {
      val es = (vertices zip adjList).flatMap { adj =>
        adj._2.map(x => (x._1, adj._1, x._2))
      }
      
      AdjListGraph(true, vertices.toVector, es: _*).asInstanceOf[this.type]
    } else this
  }
  
  def allPairsShortestPath_edgeDP = error("not implemented")
  def allPairsShortestPath_fastEdgeDP = error("not implemented")
  def allPairsShortestPath_floydwarshall = error("not implemented")
  def transitiveClosure = error("not implemented")
  
  def addDummySource() = {
    // undirected graph can be regarded as directed graph
    val s = Vertex(nVertices)
    val newVs = vertices :+ s
    val newAdjList = adjList :+ vertices.map((_, 0.0)).toList
    (new AdjListGraph(true, newVs, newAdjList).asInstanceOf[this.type], s)
  }
  
  def reweight(f: (Vertex, Vertex) => Double) = {
    val newEdges = edges.map(e => (e.src, e.dst, f(e.src, e.dst)))
    
    AdjListGraph(isDirected, vertices, newEdges: _*).asInstanceOf[this.type]
  }
}

object AdjListGraph {
  def apply(isDirected: Boolean, vs: Vector[Vertex], edges: (Vertex, Vertex, Double)*): AdjListGraph = {
    val adjList = Array.fill[List[(Vertex, Double)]](vs.length)(Nil)
    edges foreach { e =>
      val s = e._1
      val d = e._2
      val w = e._3
      if (!isDirected) {
        adjList(d) = ((s, w) :: adjList(d))
      }
      adjList(s) = ((d, w) :: adjList(s))
    }
    
    new AdjListGraph(isDirected, vs, adjList.toVector)
  }
  
  def apply(isDirected: Boolean, nVs: Int, edges: (Int, Int, Double)*): AdjListGraph = {
    val vs = (0 until nVs).map(Vertex(_)).toVector
    val es = edges.map(e => (vs(e._1), vs(e._2), e._3))
    apply(isDirected, vs, es: _*)
  }
  
/*
  def apply(isDirected: Boolean)(vs: Vector[AdjVertex], unweightedPairs: (AdjVertex, AdjVertex)*) = {
    val adjList = Array.fill[List[(AdjVertex, Double)]](vs.length)(Nil)
    unweightedPairs foreach { e =>
      val s = e._1
      val d = e._2
      if (!isDirected) {
        adjList(d.id) = ((s, 1.0) :: adjList(d.id))
      }
      adjList(s.id) = ((d, 1.0) :: adjList(s.id))
    }
    
    new AdjListGraph((vs zip adjList).map(x => new Adjacency(x._1, x._2)), isDirected)
  }
*/
}