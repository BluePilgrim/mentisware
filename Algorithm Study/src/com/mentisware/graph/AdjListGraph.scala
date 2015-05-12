package com.mentisware.graph
import com.mentisware.disjointset.DisjointSet
import com.mentisware.disjointset.ForestSet

// assume that a vertex's id is set to the index of the adjacency list

class AdjListGraph(
    val vertices: Vector[Vertex],
    val adjList: Vector[List[(Vertex, Double)]],
    directed: Boolean) extends Graph {
  require(vertices.length == adjList.length)
  type V = Vertex
  
  def isDirected = directed
  def nVertices = vertices.length
  
  def edges = (vertices zip adjList).flatMap { x =>
    val (s, l) = x
    l.map(y => Edge(s, y._1, y._2))
  }
  def nEdges = edges.length
  def edge(src: V, dst: V) = {
    adjList(src).find(_._1 == dst) match {
      case Some((v, w)) => Some(Edge(src, v, w))
      case _ => None
    }
  }
  def adjVertices(s: V) = adjList(s)
    
  def transpose() = {
    if (isDirected) {
      val es = (vertices zip adjList).flatMap { adj =>
        adj._2.map(x => (x._1, adj._1, x._2))
      }
      
      AdjListGraph(true, vertices.toVector, es: _*).asInstanceOf[this.type]
    } else this
  }
}

object AdjListGraph {
/*
  // automatic conversion of unweighted graph
  implicit def unweighted2weighted(x: (AdjVertex, AdjVertex)): (AdjVertex, AdjVertex, Double) =
    (x._1, x._2, 1.0)
*/
  
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
    
    new AdjListGraph(vs, adjList.toVector, isDirected)
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