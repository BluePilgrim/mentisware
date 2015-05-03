package com.mentisware.disjointset

//
// Disjoint Set ADT
// assume that input data contain no duplicate.
//

trait Set[T] {
  def representative: T
  def add(x: T): Set[T]
//  def isMember(x: T): Boolean
}


trait DisjointSet[T] {
  def set(x: T) : Set[T]
  def addSet(x: T): DisjointSet[T]
  def union(x: T, y: T): DisjointSet[T]
  def sets : Seq[Set[T]]
}