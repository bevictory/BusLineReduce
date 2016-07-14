package common

/**
 * Created by dgh on 15-8-5.
 * RoundRobin 分区函数
 */
import org.apache.spark._
import org.apache.spark.util.Utils
class RoundRobinPartitioner(partitions:Int,iterno:Int) extends Partitioner  {
  var iteNo:Int = iterno
  def numPartitions:Int = partitions

  /**
   * 根据key获取分区位置
   * @param key 键值
   * @return 分区位置
   */
  def getPartition(key:Any):Int = {

    key match {
      case null => 0
      case _ => roundRobin(iteNo,(key.hashCode()&Integer.MAX_VALUE)%(numPartitions*2)+1,numPartitions*2)
    }

  }
  override def equals(other: Any): Boolean = other match {
    case h: HashPartitioner =>
      h.numPartitions == numPartitions
    case _ =>
      false
  }

  override def hashCode: Int = numPartitions

  /**
   * RoundRobin 函数，每个区保存两个段
   * @param ite 迭代次数
   * @param partition 默认分区位置
   * @param partitionSum 分段总数
   * @return  分区位置
   */
  def roundRobin(ite: Int,partition:Int, partitionSum :Int ) :Int = {
    val arraySize = partitionSum/2
    var left = 0
    var locat = 0
    val up = new Array[Int](arraySize)
    val dn = new Array[Int](arraySize)
    for( i <- 0 until  arraySize ) {
      up(i) = i*2+1
      dn(i) = i*2+2
    }
    if(partition == partitionSum-1 ) {
      return (partition-1) /2
    }
    if(partition %2 ==0) {
      if(ite > (partitionSum-partition)/2) {
        left = ite - (partitionSum - partition)/2
        if(left > (partitionSum/2 - 1)) {
          left -= partitionSum/2-1
          locat = dn(left-1)
        }else {
          locat = up(partitionSum/2-left-1)
        }
      }else {
        locat = partition + ite*2
      }
    }else {
      if(ite > (partition-1)/2) {
        left = ite-(partition-1)/2
        if(left > (partitionSum/2)){
          left = left -(partitionSum/2)
          locat = up((partitionSum/2-1)-left)
        } else {
          locat = dn(left -1)
        }
      }else {
        locat = partition - ite*2

      }
    }
    (locat-1)/2
  }

}
