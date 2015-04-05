package com.mentisware.tree.vebtree

import com.mentisware.tree.SearchTree
import com.mentisware.tree.Element

// Implementation of van Emde Boas Tree

// Some implementation details
// - the initial unset value for min/max is -1, because the universe of keys is 0 to (u-1).
// - in order to deal with duplicate keys and satellite information, min/max maintain a list of satellite information.
// - contrary to min, max is duplicated, so that the sat. info list are accessible via two entries.
// - basically, summary(meta information) nodes do not need have have such list,
//   but processing nodes of both types in a uniform way provides cleaner code and faster execution.
// - when a key is duplicated, apply/search may return any data with the key.

private[vebtree] case class Slot(data: List[Element[Int]] = Nil) {
  def isSet = data != Nil
  def keys = if (data != Nil) data.map(_.key) else Nil
  def elem = data.head
  def insert(n: List[Element[Int]]) = Slot(n ::: data)
  def delete(e: Element[Int]) = {
//    val res = data.filter(_ != e) <--- multiple data could be removed
    val res = data diff List(e)
    if (res.isEmpty) null else if (res == data) this else Slot(res)
  }
}

abstract class vEBTree extends SearchTree[Int] {
  def min: Int
  def max: Int
  def univSize: Int                   // universe size
  def halfBitSize: Int                // (bit size to represent univSize) / 2
  def keys = Vector(min, max)         // relatively meaningless in this case
//  def numOfKeys = 2                   // min, max are only treated as keys

  protected def NaN = -1              // used to specify an invalid number
  
  def apply(k: Int) = this match {
    case x: vEBBaseData =>
      if (min == k) x.minSlot.elem
      else if (max == k) x.maxSlot.elem
      else error("No such key exists")
    case x: vEBNormalData =>
      if (min == k) x.minSlot.elem
      else if (max == k) x.maxSlot.elem
      else x.cluster(high(k))(low(k))
    case _ => error("No such key exists")
  }
  
  def search(k: Int) = this match {
    case x: vEBBaseData =>
      if (min == k) x.minSlot.data
      else if (max == k) x.maxSlot.data
      else error("No such key exists")
    case x: vEBNormalData =>
      if (min == k) x.minSlot.data
      else if (max == k) x.maxSlot.data
      else x.cluster(high(k)).search(low(k))
    case _ => error("No such key exists")
  }
  
  def minimum = this match {
    case x: DataSlot => if (x.minSlot.isSet) Some(x.minSlot.elem) else None
    case _ => error("No such key exists")
  }
  
  def maximum = this match {
    case x: DataSlot => if (x.maxSlot.isSet) Some(x.maxSlot.elem) else None
    case _ => error("No such key exisits")
  }

  def isMember(k: Int): Boolean = this match {
    case _: vEBBase => (k == min || k == max)
    case x: vEBNormal => (k == min || k == max || x.cluster(high(k)).isMember(low(k)))
  }
  
  def successor(k: Int): Int = this match {
    case _: vEBBase =>
      if (k == 0 && max == 1) 1 else NaN
    case x: vEBNormal =>
      if (min != NaN && k < min) min
      else {
        val clusterNum = high(k)
        val offset = low(k)
        val maxLow = x.cluster(clusterNum).max
        if (maxLow != NaN && offset < maxLow) index(clusterNum, x.cluster(clusterNum).successor(offset))
        else {
          val succCluster = x.summary.successor(clusterNum)
          if (succCluster == NaN) NaN else index(succCluster, x.cluster(succCluster).min)
        }
      }
  }
  
  def predecessor(k: Int): Int = this match {
    case _: vEBBase =>
      if (k == 1 && min == 0) 0 else NaN
    case x: vEBNormal =>
      if (max != NaN && k > max) max
      else {
        val clusterNum = high(k)
        val offset = low(k)
        val minLow = x.cluster(clusterNum).min
        if (minLow != NaN && offset > minLow) index(clusterNum, x.cluster(clusterNum).predecessor(offset))
        else {
          val predCluster = x.summary.predecessor(clusterNum)
          if (predCluster == NaN)
            if (min != NaN && k > min) min else NaN
          else
            index(predCluster, x.cluster(predCluster).max)
        }
      }
  }
  
  def insert(e: Element[Int]) = insert(e.key, List(e))
  def delete(e: Element[Int]) = delete(e.key, e, false)
  
  def isEmpty() = min == NaN && min == NaN
  
  def keyList: List[Int] = {
    var x = max
    var xs: List[Int] = Nil
    while (x != NaN) {
      xs = search(x).map(_.key) ::: xs
      x = predecessor(x)
    }
    xs
  }
  
  def validate() {
    def checkIntegrity(n: vEBTree, isIndex: Boolean): Unit = n match {
      case x: vEBBaseNoData =>
        if (!isIndex && x.min != NaN) error("index type is required")
        if (x.min > x.max) error("min/max is strange")
      case x: vEBBaseData =>
        if (isIndex) error("data type is required")
        if (x.min == NaN) error("min/max should be set")
      case x: vEBNormalNoData =>
        if (!isIndex && x.min != NaN) error("index type is required")
        for (i <- 0 until x.cluster.length) {
          if (x.summary.isMember(i) && x.cluster(i).min == NaN)
            error("summary and cluster info integrity is broken")
          checkIntegrity(x.cluster(i), isIndex || false)
        }
        checkIntegrity(x.summary, true)
      case x: vEBNormalData =>
        if (isIndex) error("data type is required")
        if (x.min == NaN) error("min/max should be set")
        if (x.maxSlot.data == Nil) error("data has been lost")
        for (i <- 0 until x.cluster.length) {
          if (x.summary.isMember(i) && x.cluster(i).min == NaN)
            error("summary and cluster info integrity is broken")
          checkIntegrity(x.cluster(i), false)
        }
        checkIntegrity(x.summary, true)
    }
    
    checkIntegrity(this, false)
  }

  
  // utility methods && internal representations
  private def high(x: Int) = x >> halfBitSize
  private def low(x: Int) = x & ((0x1 << halfBitSize) - 1)
  private def index(x: Int, y: Int) = (x << halfBitSize) + y
  
  private def insertEmpty(k: Int, data: List[Element[Int]] = Nil): vEBTree = this match {
    case _: vEBBaseNoData =>
      if (data == Nil) vEBBaseNoData(k, k)
      else {
        val newSlot = Slot(data)
        vEBBaseData(k, k, newSlot, newSlot)
      }
    case _: vEBBaseData =>
      if (data == Nil) error("element is not specified")
      else {
        val newSlot = Slot(data)
        vEBBaseData(k, k, newSlot, newSlot)
      }
    case x: vEBNormalNoData =>
      if (data == Nil) vEBNormalNoData(k, k, univSize, x.summary, x.cluster)
      else {
        val newSlot = Slot(data)
        vEBNormalData(k, k, univSize, x.summary, x.cluster, newSlot, newSlot)
      }
    case x: vEBNormalData =>
      if (data == Nil) error("element is not specified")
      else {
        val newSlot = Slot(data)
        vEBNormalData(k, k, univSize, x.summary, x.cluster, newSlot, newSlot)
      }
  }

  private def insert(k: Int, data: List[Element[Int]] = Nil): vEBTree = {
    if (data == Nil)
      this match {
        case _: DataSlot => error("No Data type operation is invoked")
        case _: vEBBaseNoData =>
          if (min == NaN) insertEmpty(k)
          else if (k < min || k > max) {
            assert(k == 0 && min == 1 || max == 0 && k == 1)
            vEBBaseNoData(0, 1)
          } else this
        case x: vEBNormalNoData =>
          if (min == NaN) insertEmpty(k)
          else if (min == k || max == k) this  // do nothing
          else {
            // if k is less than min, k is set to min and the original min should be inserted.
            val (newK, newMin) = if (k < min) (min, k) else (k, min)
            val clusterNum = high(newK)
            val offset = low(newK)
            val (newSummary, newCluster) = 
              if (x.cluster(clusterNum).min == NaN)
                (x.summary.insert(clusterNum),
                    x.cluster.updated(clusterNum, x.cluster(clusterNum).insertEmpty(offset)))
              else (x.summary, x.cluster.updated(clusterNum, x.cluster(clusterNum).insert(offset)))
            val newMax = if (newK > max) newK else max
            vEBNormalNoData(newMin, newMax, univSize, newSummary, newCluster)
          }
      }
    else
      this match {
        case x: NoDataSlot =>
          if (min != NaN) error("Data should've alrady represented with Data type")
          else insertEmpty(k, data)
        case x: vEBBaseData =>
          if (min == NaN) error("Non-data should be represented with NoData type")
          else if (k < min) {
            assert(x.minSlot == x.maxSlot)
            vEBBaseData(0, 1, Slot(data), x.minSlot)
          } else if (k > max) {
            assert(x.minSlot == x.maxSlot)
            vEBBaseData(0, 1, x.minSlot, Slot(data))
          } else {
            val newMinSlot = if (k == min) x.minSlot.insert(data) else x.minSlot
            val newMaxSlot = if (min == max) newMinSlot else if (k == max) x.maxSlot.insert(data) else x.maxSlot
            vEBBaseData(min, max, newMinSlot, newMaxSlot)
          }
        case x:vEBNormalData =>
          if (min == NaN) error("Non-data should be represented with NoData type")
          else if (k == min) {
            val newMinSlot = x.minSlot.insert(data)
            val newMaxSlot = if (min == max) newMinSlot else x.maxSlot
            vEBNormalData(min, max, univSize, x.summary, x.cluster, newMinSlot, newMaxSlot)
          } else {
            // if k is less than min, k is set to min and the original min should be inserted.
            val (newK, newMin, newMinSlot, newData) =
              if (k < min) (min, k, Slot(data), x.minSlot.data) else (k, min, x.minSlot, data)
            val clusterNum = high(newK)
            val offset = low(newK)
            val (newSummary, newCluster) = 
              if (x.cluster(clusterNum).min == NaN)
                (x.summary.insert(clusterNum),
                    x.cluster.updated(clusterNum, x.cluster(clusterNum).insertEmpty(offset, newData)))
              else (x.summary, x.cluster.updated(clusterNum, x.cluster(clusterNum).insert(offset, newData)))
            val (newMax, newMaxSlot) =
              if (newK > max) (newK, Slot(newData))
              else if (newK == max && newK != min) (max, x.maxSlot.insert(newData))
              else (max, x.maxSlot)
            vEBNormalData(newMin, newMax, univSize, newSummary, newCluster, newMinSlot, newMaxSlot)
          }
    }
  }
  
  private def delete(k: Int, e: Element[Int] = null, forceDelete: Boolean = false): vEBTree = {
    require(min != NaN)
    if (e == null) {
      this match {
        case _: DataSlot => error("No Data type operation is invoked")
        case _: vEBBaseNoData =>
          if (min == max) vEBBaseNoData(NaN, NaN)
          else if (k == 0) vEBBaseNoData(1, 1)
          else vEBBaseNoData(0, 0)
        case x: vEBNormalNoData =>
          if (min == max) {  // the tree has only one data
            assert(min == k)
            vEBNormalNoData(NaN, NaN, univSize, x.summary, x.cluster)
          } else {
            val (newK, newMin) =
              if (k == min) {
                val succ = index(x.summary.min, x.cluster(x.summary.min).min)
                (succ, succ)
              } else (k, min)
            val clusterNum = high(newK)
            val offset = low(newK)
            val newCluster = x.cluster.updated(clusterNum, x.cluster(clusterNum).delete(offset))
            val newSummary =
              if (newCluster(clusterNum).min == NaN) x.summary.delete(clusterNum) else x.summary
            val newMax =
              if (newK != max) max
              else if (newSummary.max == NaN) newMin
              else index(newSummary.max, newCluster(newSummary.max).max)
              
            vEBNormalNoData(newMin, newMax, univSize, newSummary, newCluster)
          }
      }
    } else {
      this match {
        case _: NoDataSlot => error("Data type operation is invoked")
        case x: vEBBaseData =>
          if (min == max)
            if (forceDelete) vEBBaseNoData(NaN, NaN)
            else {
              val newSlot = x.minSlot.delete(e)
              if (newSlot == x.minSlot) this
              else if (newSlot != null) vEBBaseData(min, min, newSlot, newSlot)
              else vEBBaseNoData(NaN, NaN)
            }
          else if (k == 0)
            if (forceDelete) vEBBaseData(1, 1, x.maxSlot, x.maxSlot)
            else {
              val newSlot = x.minSlot.delete(e)
              if (newSlot == x.minSlot) this
              else if (newSlot != null) vEBBaseData(min, max, newSlot, x.maxSlot)
              else vEBBaseData(1, 1, x.maxSlot, x.maxSlot)
            }
          else
            if (forceDelete) vEBBaseData(0, 0, x.minSlot, x.minSlot)
            else {
              val newSlot = x.maxSlot.delete(e)
              if (newSlot == x.maxSlot) this
              else if (newSlot != null) vEBBaseData(min, max, x.minSlot, newSlot)
              else vEBBaseData(0, 0, x.minSlot, x.minSlot)
            }
        case x: vEBNormalData =>
          if (min == max) {  // the tree has only one data
            assert(min == k)
            if (forceDelete) vEBNormalNoData(NaN, NaN, univSize, x.summary, x.cluster)
            else {
              val newSlot = x.minSlot.delete(e)
              if (newSlot == x.minSlot) this
              else if (newSlot != null) vEBNormalData(min, min, univSize, x.summary, x.cluster, newSlot, newSlot)
              else vEBNormalNoData(NaN, NaN, univSize, x.summary, x.cluster)
            }
          } else {
            val (res, newK, newMin, newMinSlot, newForceDelete) = 
              if (k == min) {
                if (forceDelete) {                
                  val succ = index(x.summary.min, x.cluster(x.summary.min).min)
                  x.cluster(x.summary.min) match {
                    case n: DataSlot => (null, succ, succ, n.minSlot, true)
                    case _ => error("data type is required")
                  }
                } else {
                  val newSlot = x.minSlot.delete(e)
                  if (newSlot == x.minSlot)
                    (this, NaN, NaN, null, false)
                  else if (newSlot != null)
                    (vEBNormalData(min, max, univSize, x.summary, x.cluster, newSlot, x.maxSlot),
                        NaN, NaN, null, false)
                  else {
                    val succ = index(x.summary.min, x.cluster(x.summary.min).min)
                    x.cluster(x.summary.min) match {
                      case n: DataSlot => (null, succ, succ, n.minSlot, true)
                      case _ => error("data type is required")
                    }
                  }
                }
              } else (null, k, min, x.minSlot, forceDelete)
            
            if (res == null) {
              val clusterNum = high(newK)
              val offset = low(newK)
              val newCluster =
                x.cluster.updated(clusterNum, x.cluster(clusterNum).delete(offset, e, newForceDelete))
              val newSummary =
                if (newCluster(clusterNum).min == NaN) x.summary.delete(clusterNum) else x.summary
              val (newMax, newMaxSlot) =
                if (newK != max) (max, x.maxSlot)
                else if (newSummary.max == NaN) (newMin, newMinSlot)
                else
                  newCluster(newSummary.max) match {
                    case n: DataSlot => (index(newSummary.max, newCluster(newSummary.max).max), n.maxSlot) 
                    case _ => error("data type is required")
                  }
                  
              vEBNormalData(newMin, newMax, univSize, newSummary, newCluster, newMinSlot, newMaxSlot)                
            } else res
          }
      }
    }
  }

//  println("half bit size = " + halfBitSize + " univ size = " + univSize)
//  require(0x1 << (halfBitSize * 2 + 1) >= univSize)
}

trait NoDataSlot
trait DataSlot {
  def minSlot: Slot
  def maxSlot: Slot
}

sealed abstract class vEBBase extends vEBTree {
  def univSize = 2
  def halfBitSize = 0
  def children = null
  override def numOfChildren = 0  
}

case class vEBBaseNoData(min: Int, max: Int) extends vEBBase with NoDataSlot
case class vEBBaseData(min: Int, max: Int, minSlot: Slot, maxSlot: Slot) extends vEBBase with DataSlot

abstract class vEBNormal extends vEBTree {
  def cluster: Vector[vEBTree]
  def summary: vEBTree
  val halfBitSize = getP(univSize-1) / 2

  def children = cluster :+ summary    // a node has cluster nodes and summary
  override def numOfChildren = 1 + cluster.length  
  def getP(n: Int): Int = if (n == 0) 0 else getP(n >> 1) + 1

}

case class vEBNormalNoData(min: Int, max: Int,
    univSize: Int, summary: vEBTree, cluster: Vector[vEBTree]) extends vEBNormal with NoDataSlot
case class vEBNormalData(min: Int, max: Int,
    univSize: Int, summary: vEBTree, cluster: Vector[vEBTree],
    minSlot: Slot, maxSlot: Slot) extends vEBNormal with DataSlot

    

object vEBTree {
  private def getP(n: Int): Int = if (n == 0) 0 else getP(n >> 1) + 1

  def createEmptyTree(u: Int): vEBTree = {
    def NaN = -1
    val bitSize = getP(u - 1)
    if (u == 2) vEBBaseNoData(NaN, NaN)
    else {
      val halfBitSize = bitSize / 2
      val clusterLen = u >> (halfBitSize)
      val summary = createEmptyTree(clusterLen)
      val cluster = Vector.fill(clusterLen)(createEmptyTree(0x1 << halfBitSize))
      vEBNormalNoData(NaN, NaN, u, summary, cluster)
    }
  }
  
  def build(u: Int)(xs: Seq[Element[Int]]): vEBTree = {
    // augment 'u' to be a power of 2
    val pow = getP(u - 1)
    val univSize = 0x1 << pow
    
//    println("univ size = " + univSize)
    val emptyTree = createEmptyTree(univSize)
    (emptyTree /: xs) (_.insert(_))
  }
}