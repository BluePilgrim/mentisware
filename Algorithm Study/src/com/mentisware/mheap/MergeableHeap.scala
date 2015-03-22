package com.mentisware.mheap

// for mergeable heap, the element should have a referenced to a corresponding satellite data.
// it is recommended that Element is extended to have such pointers.
abstract class Element[T: Ordering] {
  def key: T
  def updateKey(k: T): T
  override def toString = key.toString
}

// The representative interface for mergeable heap types.
// This time, they are mutable.
trait MergeableHeap[T] {
  def isMinHeap: Boolean                 // true for min heap, false for max heap
  def isEmpty: Boolean
  def getSize(): Int
  
  def insert(e: Element[T])
  def head: Option[Element[T]]           // minimum for min heap and maximum for max heap
  def extractHead(): Option[Element[T]]
  def union(that: this.type)              // add the components into this heap
  def updateKey(e: Element[T], k: T)     // decrease key for min heap, increase key for max heap
  def delete(e: Element[T])
  
  def error(m: String) = throw new NoSuchElementException(m)
  def validate()

  implicit def elem2key(e: Element[T]): T = e.key
}