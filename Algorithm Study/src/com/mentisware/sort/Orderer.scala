package com.mentisware.sort

object Orderer {
  import scala.collection.mutable.ArrayBuffer
  import Ordering.Implicits._
  import Numeric.Implicits._
  
  def selectSmallest[T: Ordering](xs: List[T], k: Int): T = {
    val buf = xs.to[ArrayBuffer]
    
    def partition(start: Int, end: Int): Int = {
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
      
      // there are four areas :
      //     (less than area (start to lessArea),
      //      greater or equal area (lessArea + 1 to cur),
      //      not visited (cur + 1 to end - 1),
      //      pivot (end))
      while (cur < end) {
        val cVal = buf(cur)
        if (cVal < pivot) {
          lessArea += 1
          buf(cur) = buf(lessArea)
          buf(lessArea) = cVal
        }
        cur += 1
      }
      
      // put pivot into just right of less Area
      lessArea += 1
      buf(end) = buf(lessArea)
      buf(lessArea) = pivot
      lessArea
    }

    def selSmallest(start:Int, end: Int, k: Int): T = {
      if (start >= end) buf(start)
      else {
        val p = partition(start, end)
      
        if (p == k) buf(p)
        else if (p > k) selSmallest(start, p - 1, k)
        else selSmallest(p + 1, end, k)
      }
    }
    
    selSmallest(0, buf.length - 1, k - 1)    // the internal representation starts from zero.
  }
  
  def selectWeightedSum[T: Numeric](xs: List[T], ws: List[Double], rate: Double): Option[T] = {
    require (rate >= 0 && rate <= 100)    // the rate is in %
    
    val buf = xs.to[ArrayBuffer]
    val wBuf = ws.to[ArrayBuffer]
    val weightSum = (0.0 /: ws) (_ + _)
    val targetSum = weightSum * rate / 100
    
    def partition(start: Int, end: Int): Int = {
      def setPivotAtEnd() {      // select the pivot with the weighted median of (start, mid, end) and move the pivot to end
        if (start + 2 > end) {
          val mid = (start + end) / 2
          val (sVal, mVal, eVal) = (buf(start), buf(mid), buf(end))
          val (sw, mw, ew) = (wBuf(start), wBuf(mid), wBuf(end))
          
          if (sVal > mVal && sVal > eVal) {
            //swap start with end
            buf(start) = eVal
            buf(end) = sVal
            
            wBuf(start) = ew
            wBuf(end) = sw
          } else if (mVal > sVal && mVal > eVal) {
            // swap mid with end
            buf(mid) = eVal
            buf(end) = mVal
            
            wBuf(mid) = ew
            wBuf(end) = mw
          }
        }
      }
      
      setPivotAtEnd()
      
      val pivot = buf(end)
      val wPivot = wBuf(end)
      
      var lessArea = start - 1
      var cur = start
      
      // there are four areas :
      //     (less than area (start to lessArea),
      //      greater or equal area (lessArea + 1 to cur),
      //      not visited (cur + 1 to end - 1),
      //      pivot (end))
      while (cur < end) {
        val cVal = buf(cur)
        val cw = wBuf(cur)
        if (cVal < pivot) {
          lessArea += 1
          buf(cur) = buf(lessArea)
          wBuf(cur) = wBuf(lessArea)
          buf(lessArea) = cVal
          wBuf(lessArea) = cw
        }
        cur += 1
      }
      
      // put pivot into just right of less Area
      lessArea += 1
      buf(end) = buf(lessArea)
      buf(lessArea) = pivot
      wBuf(end) = wBuf(lessArea)
      wBuf(lessArea) = wPivot
      lessArea
    }
    
    def selWeightedSum(start: Int, end: Int, wBase: Double): Option[T] = {
      if (start >= end) {
        val x = buf(start)
        if (wBase < targetSum) Some(x)
        else None
      } else {
        val p = partition(start, end)
        val pVal = buf(p)
        val wp = wBuf(p)
        
        // calculate the weighted sum of the less part
        val lessWSum = (0.0 /: wBuf.slice(start, p)) (_+_)
        
        if (wBase + lessWSum >= targetSum) selWeightedSum(start, p - 1, wBase)
        else {
          val greaterWSum = (0.0 /: wBuf.slice(p + 1, end + 1)) (_+_)
          
          if (wBase + lessWSum + wp >= targetSum) Some(pVal)
          else selWeightedSum(p + 1, end, wBase + lessWSum + wp)
        }
      }
    }
    
    selWeightedSum(0, buf.length - 1, 0.0)
  }
}