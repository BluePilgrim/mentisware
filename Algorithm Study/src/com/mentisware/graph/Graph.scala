package com.mentisware.graph
import com.mentisware.disjointset.DisjointSet

abstract class Vertex

// edge weight, for un-weighted graph, the valude is undefined or set to 1 for convenience.
case class Edge(src: Vertex, dst: Vertex, w: Double)


trait Graph {
  type V <: Vertex
  trait PredGraph {
    def pred(v: V): V
    def path(src: V, dst: V): List[V]
    def connectedSets: DisjointSet[V]
  }
  trait TimeStamp {
    def discover(v: V): Int
    def finish(v: V): Int
  }
  
  def isDirected: Boolean
  def vertices: Seq[V]
  def nVertices: Int
  def edges: Seq[Edge]
  def nEdges: Int
  
  def bfsGraph(ss: V*): (PredGraph, TimeStamp)            // only consider the first vertex
  def dfsGraph(ss: V*): (PredGraph, TimeStamp)
  def sortTopologically(): Vector[V]
  def transpose(): this.type                              // edges are reversed
  def stronglyConnectedComponents: DisjointSet[V]
  
  def MST_kruskal: (Seq[Edge], Double)                    // minimum spanning tree by Kruskal's algorithm
  def MST_prim: (Seq[Edge], Double)                       // minimum spanning tree by prim's algorithm
  
  def error(m: String) = throw new NoSuchElementException(m)
}