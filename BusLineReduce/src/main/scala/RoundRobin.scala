package main.scala

/**
 * Created by dgh on 15-7-31.
 */
object RoundRobin {
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
  /*def mapFunc[T](iteration: Iterator[T]):Iterator[(T,T)] ={
    val res = List[(T,T)]
    while(iteration.hasNext) {

    }
    return
  }*/
  def main(args : Array[String]): Unit = {
    for(i <- 0 to 9 ) {
      print(roundRobin(i,4,10))
      print(" ")
    }

  }
}
