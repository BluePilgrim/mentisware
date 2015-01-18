package com.mentisware.sort

import Ordering.Implicits._
import scala.collection.mutable.ArrayBuffer

case class Interval[T: Ordering](val low: T, val high: T) {
  def overlap(that: Interval[T]): Boolean = this.low <= that.high && that.low <= this.high
  def intersect(that: Interval[T]): Option[Interval[T]] =
    if (this overlap that)
      Some(Interval((if (this.low < that.low) that.low else this.low),
          (if (this.high < that.high) this.high else that.high)))
    else None
            
  override def toString = "[" + low + ", " + high + "]"
}


object Interval {
  class IntervalOrdering[T: Ordering](implicit ordering: Ordering[T]) extends Ordering[Interval[T]] {
    def compare(x: Interval[T], y: Interval[T]): Int = ordering.compare(x.low, y.low)
  }

  implicit object IntIntervalOrdering extends IntervalOrdering[Int]
  implicit object DoubleIntervalOrdering extends IntervalOrdering[Double]

  def sort[T : Ordering](xs: List[Interval[T]])(implicit ordering: Ordering[Interval[T]]): List[Interval[T]] = {
    val buf = xs.to[ArrayBuffer]
    
    def partition(start: Int, end: Int): (Int, Int) = {
      def setPivotAtEnd() {      // select the pivot with the median of (start, mid, end) and move the pivot to end
        if (start + 2 > end) {
          val mid = (start + end) / 2
          val (sVal, mVal, eVal) = (buf(start), buf(mid), buf(end))

          if (sVal > mVal && sVal > eVal) {
            //swap start with end
            buf(start) = eVal
            buf(end) = sVal
          } else if (mVal > sVal && mVal > eVal) {
            // swap mid with end
            buf(mid) = eVal
            buf(end) = mVal
          }
        }
      }
      
      setPivotAtEnd()
      
      val pivot = buf(end)
      var lessArea = start - 1
      var cur = start
      var overlapInterval = pivot
      var overlapExist = true
      
      // there are four areas :
      //     (less than area (start to lessArea),
      //      greater or equal area (lessArea + 1 to cur),
      //      not visited (cur + 1 to end - 1),
      //      pivot (end))
      while (cur < end) {
        val cVal = buf(cur)
        
        if (overlapExist) {
          overlapInterval.intersect(cVal) match {
            case None => overlapExist = false
            case Some(x) => overlapInterval = x
          }
        }
        
        if (cVal < pivot) {
          lessArea += 1
          buf(cur) = buf(lessArea)
          buf(lessArea) = cVal
        }
        
        cur += 1
      }
      
      // put pivot into just right of less Area
      buf(end) = buf(lessArea + 1)
      buf(lessArea + 1) = pivot

      // now the layout is : (less area, pivot, great or equal area)
      // collect overlapping elements around the overlapping area
      // (separate less area, overlapping less area, pivot, overlapping greater area, greater area)
      // There should be an intersection among overlapping intervals (IMPORTANT!!!)
      // `lessArea` is the pivot
      var overlappingLess, overlappingGreater = lessArea + 1
      if (overlapExist) {
        cur = lessArea
        while (cur >= start) {
          val x = buf(cur)
          assert(x < pivot)
          if (overlapInterval overlap x) {
            overlappingLess -= 1
            buf(cur) = buf(overlappingLess)
            buf(overlappingLess) = x
          }
          cur -= 1
        }
  
        cur = lessArea + 2
        while (cur <= end) {
          val x = buf(cur)
          assert(pivot <= x)
          if (overlapInterval overlap x) {
            overlappingGreater += 1
            buf(cur) = buf(overlappingGreater)
            buf(overlappingGreater) = x
          }
          cur += 1
        }
      }
      
      (overlappingLess, overlappingGreater)
    }
    
    def qSort(start: Int, end: Int) {
      if (start < end) {
        val (pivotLess, pivotGreater) = partition(start, end)
        qSort(start, pivotLess-1)
        qSort(pivotGreater+1, end)
      }
    }
    
    qSort(0, buf.length-1)
    
    buf.toList
  }
}