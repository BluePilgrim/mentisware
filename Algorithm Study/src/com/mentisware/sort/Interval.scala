package com.mentisware.sort

case class Interval[T: Ordering] (val start: T, val end: T) {
  import Ordering.Implicits._

  def overlap(that: Interval[T]): Boolean = (
        this.start < that.start && this.end >= that.start ||   // (a, b), (c, d) -> (a, c, b, d) or (a, c, d, b)
        this.start >= that.start && this.start <= that.end     // (a, b), (c, d) -> (c, a, d, b) or (c, a, b, d)
      )
            
  override def toString = "[" + start + ", " + end + "]"
}