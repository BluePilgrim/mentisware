package com.mentisware.disjointset

//
// Disjoint Set ADT
//

trait Set[T] {
  def representative: T
  def add(x: T): Set[T]
//  def isMember(x: T): Boolean
}


trait DisjointSet[T] {
  def set(x: T) : Set[T]
  def union(x: T, y: T): DisjointSet[T]
  def sets : Seq[Set[T]]
}