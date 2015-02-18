package com.mentisware.tree

// base trait for node data
trait Node[T] {
  def key: T
  override def toString = key.toString
}

// template for search tree
trait SearchTree[T, E] {
  def elem: E
  def key: T
  def children: Vector[SearchTree[T, E]]
  def numOfChildren: Int
  
  def search(k: T): SearchTree[T, E]
  def minimum: Option[E]
  def maximum: Option[E]
  
  def insert(n: E): SearchTree[T, E]
  def delete(n: E): SearchTree[T, E]
  
  def isEmpty(): Boolean
  
  def keyList: List[T]
  
  def error(m: String) = throw new NoSuchElementException(m)
}