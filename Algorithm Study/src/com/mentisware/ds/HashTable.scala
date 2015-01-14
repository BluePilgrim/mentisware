package com.mentisware.ds

import scala.collection.mutable.ArrayBuffer

class HashTable[Key <: Any, Entry](val size: Int, val hash: (Int => Int)) {
  case class HashEntry(key: Key, data: Entry)

  private val table = ArrayBuffer.fill[List[HashEntry]](size)(Nil)

  private def toInt(k: Key): Int = k match {
    case i: Int => i
    case s: String => math.abs((0 /: s)(_ * 37 + _))
    case _ => throw new IllegalArgumentException("The value cannot be transformed to integer.")
  }
  
  override def toString = {
    var str = ""
    for (i <- 0 until size) {
      str += "slot " + i + ": " + table(i) + "\n"
    }
    str
  }
  
  def search(k: Key): Option[Entry] = table(hash(toInt(k))).find(_.key == k) match {
    case Some(x) => Some(x.data)
    case None => None
  }
  
  def delete(k: Key): Option[Entry] = {
    search(k) match {
      case None => None
      case entry =>
        val slotNum = hash(toInt(k))
        table(slotNum) = table(slotNum).filterNot(_.key == k)
        entry
    }
  }
  
  def insert(k: Key, d: Entry): Option[Entry] = {
    val slotNum = hash(toInt(k))

    delete(k) match {
      case None =>
        table(slotNum) = HashEntry(k, d) :: table(slotNum)
        None
      case entry =>
        table(slotNum) = HashEntry(k, d) :: table(slotNum)
        entry
    }
  }
}