package com.mentisware.tree

// base trait for node data
abstract class Element[T] {
  def key: T
  override def toString = key.toString
}

// template for search tree
trait SearchTree[T] {
  def keys: Vector[T]
  def numOfKeys = keys.length
  def children: Vector[SearchTree[T]]
  def numOfChildren = children.length
  
  def apply(k: T): Element[T]
  
  def minimum: Option[Element[T]]
  def maximum: Option[Element[T]]
  
  def insert(e: Element[T]): SearchTree[T]
  def delete(e: Element[T]): SearchTree[T]
  
  def isEmpty(): Boolean
  
  def keyList: List[T]
  
  def error(m: String) = throw new NoSuchElementException(m)
  def validate()
}