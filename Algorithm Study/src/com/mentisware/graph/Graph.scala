package com.mentisware.graph

abstract class Vertex

// edge weight, for un-weighted graph, the valude is undefined or set to 1 for convenience.
case class Edge(src: Vertex, dst: Vertex, w: Double)


trait Graph {
  type V <: Vertex
  trait ParentTree {
    def parent(v: V): V
    def path(src: V, dst: V): List[V]
  }
  
  def isDirected: Boolean
  def vertices: Seq[Vertex]
  def nVertices: Int
  def edges: Seq[Edge]
  def nEdges: Int
  
  def bfsTree(s: V): ParentTree    // breadth first search returns a BFS tree
  
  def error(m: String) = throw new NoSuchElementException(m)
}