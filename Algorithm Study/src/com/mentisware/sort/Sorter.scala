package com.mentisware.sort

object Sorter {
  import scala.collection.mutable.ArrayBuffer
  import Ordering.Implicits._
  
  def insertionSort0[T : Ordering](xs: List[T]): List[T] = {
    val buf = xs.to[ArrayBuffer]
    val bufLen = buf.length
    
    for (i <- (1 until bufLen)) {
      val key = buf(i)
      var j = i - 1
      while (j >= 0 && key < buf(j)) {
        buf(j+1) = buf(j)
        j -= 1
      }
      buf(j+1) = key
    }
    
    List(buf: _*)
  }
  
  def insertionSort[T : Ordering](xs: List[T]): List[T] = {
    def insert(zs: List[T], k: T): List[T] = zs match {
      case Nil => List(k)
      case x :: ys => if (k < x) k :: zs else x :: insert(ys, k)
    }
    
    xs match {
      case Nil => Nil
      case x :: ys => insert(insertionSort(ys), x)
    }
  }
  
  def selectionSort0[T : Ordering](xs: List[T]): List[T] = {
    val buf = ArrayBuffer(xs: _*)
    val len = buf.length
    
    for (i <- (0 until len)) {
      var min = buf(i)
      var minIdx = i
      for (j <- (i+1 until len)) {
        if (buf(j) < min) {
          min = buf(j)
          minIdx = j
        }
      }
      if (minIdx != i) {
        buf(minIdx) = buf(i)
        buf(i) = min
      }
    }
    
    buf.toList
  }
  
  def selectionSort[T : Ordering](xs: List[T]): List[T] = {
    def extractMax(xs: List[T]): List[T] =
      (List(xs.head) /: xs.tail) {
        (zs, x) => if (zs.head < x) x :: zs else zs.head :: x :: zs.tail
      }
    def sSort(xs: List[T], ss: List[T]): List[T] = {
      if (xs != Nil) {
        val res = extractMax(xs)
        sSort(res.tail, res.head :: ss)
      } else ss
    }

    sSort(xs, Nil)
  }
  
  def mergeSort0[T : Ordering](xs: List[T]): List[T] = {
    val buf = ArrayBuffer(xs: _*)

    def mSort(start: Int, end: Int) {
      if (start >= end) return
      
      val bufLen = end - start + 1
      val mid = start + bufLen / 2
      mSort(start, mid - 1)        // sort the first half
      mSort(mid, end)      // sort the last half

      val tempBuf = new ArrayBuffer[T](bufLen)
      buf.copyToBuffer(tempBuf)
      var leftIdx = start
      var rightIdx = mid

      for (i <- (start to end)) {
        if (rightIdx > end || (leftIdx < mid && tempBuf(leftIdx) < tempBuf(rightIdx))) {
          buf(i) = tempBuf(leftIdx)
          leftIdx += 1
        } else {
          buf(i) = tempBuf(rightIdx)
          rightIdx += 1
        }
      }
      assert(leftIdx == mid && rightIdx == end + 1)
    }
    
    mSort(0, buf.length - 1)
    List(buf: _*)
  }

  def mergeSort[T : Ordering](xs: List[T]): List[T] = {
    def merge(ys: List[T], zs: List[T]): List[T] = (ys, zs) match {
      case (Nil, _) => zs
      case (_, Nil) => ys
      case _ =>
        if (ys.head < zs.head) ys.head :: merge(ys.tail, zs) else zs.head :: merge(ys, zs.tail)
    }
    
    xs match {
      case Nil => Nil
      case x :: Nil => xs
      case _ =>
        val (ys, zs) = xs.splitAt(xs.length/2)
        merge(mergeSort(ys), mergeSort(zs))
    }     
  }
  
  def heapSort[T: Ordering](xs: List[T]): List[T] = {
    val heap = new Heap(xs)
    
    def putMaxToList(ss: List[T]): List[T] = heap.extractHead match {
      case Some(x) => putMaxToList(x :: ss)
      case None => ss
    }
    putMaxToList(Nil)
  }
  
  def quickSort[T: Ordering](xs: List[T]): List[T] = {
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
    
    def qSort(start: Int, end: Int) {
      if (start < end) {
        val pivot = partition(start, end)
        qSort(start, pivot-1)
        qSort(pivot+1, end)
      }
    }
    
    qSort(0, buf.length-1)
    
    buf.toList
  }
  
  def fuzzySort[T: Ordering](xs: List[Interval[T]]): List[Interval[T]] = {
    val buf = xs.to[ArrayBuffer]
    
    def partition(start: Int, end: Int): (Int, Int) = {
      def setPivotAtEnd() {      // select the pivot with the median of (start, mid, end) and move the pivot to end
        if (start + 2 > end) {
          val mid = (start + end) / 2
          val (sVal, mVal, eVal) = (buf(start), buf(mid), buf(end))

          if (sVal.low > mVal.low && sVal.low > eVal.low) {
            //swap start with end
            buf(start) = eVal
            buf(end) = sVal
          } else if (mVal.low > sVal.low && mVal.low > eVal.low) {
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
        
        if (cVal.low < pivot.low) {
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
          assert(x.low < pivot.low)
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
          assert(pivot.low <= x.low)
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
  
  
  def countingSort(xs: List[Int], maxNum: Int = 10000): List[Int] = {
    import scala.collection.mutable.ListBuffer
    
    val counts = ArrayBuffer.fill(maxNum + 1)(0)
    val result = ArrayBuffer.fill(xs.length)(0)
    
    def checkCounts(list: List[Int]) {
      list match {
        case Nil => return
        case x :: ys => counts(x) += 1; checkCounts(ys)
      }
    }
    
    def accumulateCounts(k: Int) {
      if (k <= maxNum) {
        counts(k) += counts(k-1)
        accumulateCounts(k + 1)
      }
    }
    
    def cSort(list: List[Int]) {
      list match {
        case Nil => return
        case x :: ys =>      // update backward
          cSort(ys)
          counts(x) -= 1
          result(counts(x)) = x  // index starts from 0
      }      
    }
    
    checkCounts(xs)
    accumulateCounts(1)
    cSort(xs)
    
    result.toList
  }
  
  def radixSort(xs: List[Int], maxNum: Int): List[Int] = {
    var src = xs.to[ArrayBuffer]
    var dest = ArrayBuffer.fill(xs.length)(0)
    val length = xs.length
    
    def getRadix = {
      def getLog(x: Int): Int = if (x > 0) getLog(x >> 1) + 1 else 0
      val b = getLog(maxNum)
      val r = getLog(length)
      val radix = if (b < r) b else r
      (b, if (radix < 4) 4 else radix)
    }
    val (base, radix) = getRadix
    
    def getK = {
      def getExp(x: Int): Int = if (x > 0) getExp(x - 1) << 1 else 1
      getExp(radix)
    }
    val k = getK
    val filter = k - 1         

    for (i <- (0 until (base + radix -1) / radix); offset = i * radix; flt = filter << offset) {
      val counts = ArrayBuffer.fill(k+1)(0)
      def filteredVal(x: Int) = (x & flt) >> offset
 
      def checkCounts() {
        src foreach { x =>
          counts(filteredVal(x)) += 1  
        }
      }
    
      def accumulateCounts(i: Int) {
        if (i <= k) {
          counts(i) += counts(i - 1)
          accumulateCounts(i + 1)
        }
      }
    
      def cSort(i: Int) {
        if (i < length) {
            cSort(i + 1)
            val x = src(i)
            val v = filteredVal(x)
            counts(v) -= 1
            dest(counts(v)) = x  // index starts from 0
        }
      }  
      
      checkCounts()
      accumulateCounts(1)
      cSort(0)
      
      val tmp = src
      src = dest
      dest = tmp
    }

    src.toList
  }
  
  def bucketSort(xs: List[Int], maxNum: Int): List[Int] = {
    val buckets = ArrayBuffer.fill[List[Int]](xs.length)(Nil)

    def bucketIndex(x: Int) = x / (maxNum + 1)
    
    def insert(xs: List[Int], x: Int): List[Int] = xs match {
      case Nil => List(x)
      case y :: ys => if (x <= y) x :: xs else y :: insert(ys, x)
    }
    
    xs foreach { x =>
      buckets(bucketIndex(x)) = insert(buckets(bucketIndex(x)), x)
    }
    
    (buckets flatMap (x => x)).toList
  }
}