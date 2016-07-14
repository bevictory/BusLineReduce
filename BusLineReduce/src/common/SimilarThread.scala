package common

import breeze.numerics._
import org.apache.spark.Logging

/**
 * Created by dgh on 15-8-6.
 * 块间块内比较去重
 */
class SimilarThread(var segments:Array[Segment],var start:Int,var end:Int,var location:Int,var threshold:Int) extends  Runnable with Logging{
  def this() {
    this(null,0,0,0,0)
  }

  /**
   * 分区内去重
   */
  def run(): Unit = {
    var j = start
    var compareSize = end
    var compareSegment = segments(location)
    logInfo("thread start from "+start +" end at " + end.toString)
    while(j < compareSize) {
      if (SimilarThread.similarNo == 2) {
        //logInfo("thread end at " + j.toString)
        return
      }

      if (isInSimilar(compareSegment, segments(j))) {
        if( j >=1 && j < compareSize-1 && location >= 1 && segments.length -1 >location) {
          val compare = new Array[Segment](3)
          for( i <- 0 to 2) {
            compare(i) = segments(j+i-1)
          }
          val segment = new Array[Segment](3)
          for(i<-0 to 2) {
            segment(i) = segments(location + i-1)
          }
          if (compareSegment.isSimilarTo(segments(j), threshold)&&IsSimilarLine.isSimilar(segment,compare)) {
            //if (compareSegment.isSimilarTo(segments(j), threshold)) {
            compareSegment.similarBusNo = segments(j).busNo
            //println(segment.busNo.toString)
            logInfo("find similar at "+j.toString)
            segments(location).similarBusNo = segments(j).busNo
            //SimilarThread.similarNo = 2
            j = compareSize
          }
        }else {
          if (compareSegment.isSimilarTo(segments(j), threshold)) {
            compareSegment.similarBusNo = segments(j).busNo
            //println(segment.busNo.toString)
            logInfo("find similar at "+j.toString)
            segments(location).similarBusNo = segments(j).busNo
            //SimilarThread.similarNo = 2
            j = compareSize
          }
        }
      }

      j+=1
    }
    //segments(location) = compareSegment
  }

  /**
   * 判断线段是否在去重范围内
   * @param segment 去重线段
   * @param compareSegment 比较线段
   * @return ture or false
   */
  def isInSimilar(segment:Segment,compareSegment:Segment):Boolean ={
    val longSub =segment.middleLong-compareSegment.middleLong
    val latSub =segment.middleLat - compareSegment.middleLat
    if((compareSegment.similarBusNo==0) &&(longSub<0.001 &&longSub > (-0.001))&& (latSub < 0.001 &&latSub > (-0.001)) ) return true else false
    //if((longSub<0.001 &&longSub > (-0.001))&& (latSub < 0.001 &&latSub > (-0.001)) ) return true else false
  }

  /**
   * 去重线程
   * @param segments 分区线段
   * @param compareSize 比较大小
   * @param location 去重线段位置
   * @param threshold 阀值
   */
  def thread(segments:Array[Segment],compareSize:Int,location:Int, threshold:Int): Unit = {
    var thr = new SimilarThread(segments,0,compareSize,location,threshold)
    thr.run()
    /*var threads:Array[Thread] = Array(new Thread(),new Thread(),new Thread(),new Thread())
    for( i<- 0 to 1) {
      if(i!=1) threads(i)=new Thread(new SimilarThread(segments,i*compareSize/2,(i+1)*compareSize/2,location,threshold))
      if(i==1) threads(i)=new Thread(new SimilarThread(segments,i*compareSize/2,compareSize,location,threshold))
      threads(i).start()
    }
    for(i <- 0 to 1) {
      threads(i).join()
    }*/


  }

}
object SimilarThread {
  var similarNo = 1
  def thread(segments:Array[Segment],compareSize:Int,location:Int, threshold:Int) {
    val similarThread = new SimilarThread()
    similarThread.thread(segments,compareSize,location,threshold)
  }
  def main (args: Array[String]) {
    similarNo = 1
    //thread(Array(("1","1,2,1,2,1,2,0"),("2","5,6,5,6,5,6,0"),("11","2,3,2,3,2,3,0"),("9","5,6,5,6,5,6,0"),("4","3,4,3,4,3,4,0"),("5","1,2,1,2,1,2,0"),
     // ("10","2,3,2,3,2,3,0"),("6","5,6,5,6,5,6,0"),("7","1,2,1,2,1,2,0"),("8","3,4,3,4,3,4,0"),("3","1,2,1,2,1,2,0"),("12","2,3,2,3,2,3,0")),12,1,50)

  }
}
