package common

import breeze.numerics.abs
import org.apache.spark.Logging

/**
 * Created by dgh on 15-8-11.
 * 块间比较去重，块内不去重
 * segments 待去重线段所在块
 * compareSegments  比较线段所在块
 * start 比较开始位置
 * end 比较结束位置
 * location 待去重线段所在位置
 * threshold 阀值
 */
class BlockSimilarThread(segments:Array[Segment],compareSegments:Array[Segment],var start:Int,var end:Int,var location:Int,var threshold:Int) extends  Runnable with Logging{
  def this() {
    this(null,null,0,0,0,0)
  }

  /**
   * 线程执行去重算法，如果线段相似，修改similarBusNo
   * compareSegment 待去重线段
   * segments 线段数组
   */
  def run(): Unit = {
    var j = start
    var compareSize = end
    var compareSegment = segments(location)
    //logInfo("thread start from "+start +" end at " + end.toString)
    while(j < compareSize) {
      if(BlockSimilarThread.similarNo == 2) {
        //logInfo("thread end at " + j.toString)
        return
      }
      if(isInSimilar(compareSegment,compareSegments(j))) {
        if( j >=1 && j < compareSize-1 && location >1 && segments.length -1> location ) {
          val compare = new Array[Segment](3)
          for( i <- 0 to 2 ){
            compare(i) = compareSegments(j+i-1)
          }
          val segment = new Array[Segment](3)
          for( i<-0  to 2) {
            segment(i) = segments(location+i-1)
          }
          //compare={compareSegments(j-1),compareSegments(j),compareSegments(j+1)}
          if (compareSegment.isSimilarTo(compareSegments(j), threshold)&&IsSimilarLine.isSimilar(segment,compare)) {
            //if (compareSegment.isSimilarTo(compareSegments(j), threshold)) {
            compareSegment.similarBusNo = compareSegments(j).busNo
            //println(segment.busNo.toString)
            //logInfo("find similar at "+j.toString)
            segments(location).similarBusNo = compareSegments(j).busNo
            //BlockSimilarThread.similarNo = 2
            j = compareSize
          }
        }else {
          if (compareSegment.isSimilarTo(compareSegments(j), threshold)) {
            compareSegment.similarBusNo = compareSegments(j).busNo
            //println(segment.busNo.toString)
            //logInfo("find similar at "+j.toString)
            segments(location).similarBusNo = compareSegments(j).busNo
            //BlockSimilarThread.similarNo = 2
            j = compareSize
          }
        }
      }

      j+=1
    }
    //segments(location) = compareSegment
  }

  /**
   * 判断比较线段是否在去重线段相似比较范围
   * @param segment 去重线段
   * @param compareSegment 比较线段
   * @return 在相似范围返回true
   */
  def isInSimilar(segment:Segment,compareSegment:Segment):Boolean ={
    val longSub =segment.middleLong-compareSegment.middleLong
    val latSub =segment.middleLat - compareSegment.middleLat
    //if((longSub<0.001 &&longSub > (-0.001))&& (latSub < 0.001 &&latSub > (-0.001)) ) return true else false
    if((compareSegment.similarBusNo==0)&&(longSub<0.001 &&longSub > (-0.001))&& (latSub < 0.001 &&latSub > (-0.001)) ) return true else false
  }

  /**
   * 创建四个线程比较去重
   * @param segments 去重线段所在块
   * @param compareSegments 比较线段所在块
   * @param compareSize 比较范围
   * @param location 去重线段所在位置
   * @param threshold 阀值
   */
  def thread(segments:Array[Segment],compareSegments:Array[Segment],compareSize:Int,location:Int, threshold:Int): Unit = {
    val thr =new BlockSimilarThread(segments,compareSegments,0,compareSize,location,threshold)
    thr.run()
    /*var threads:Array[Thread] = Array(new Thread(),new Thread(),new Thread(),new Thread())

    for( i<- 0 to 3) {
      if(i!=3) threads(i)=new Thread(new BlockSimilarThread(segments,compareSegments,i*compareSize/4,(i+1)*compareSize/4,location,threshold))
      if(i==3) threads(i)=new Thread(new BlockSimilarThread(segments,compareSegments,i*compareSize/4,compareSize,location,threshold))
      threads(i).start()
    }
    for(i <- 0 to 3) {
      threads(i).join()
    }*/
  }

}

/**
 * 单例对象
 */
object BlockSimilarThread{
   var similarNo = 1
   def thread(segments:Array[Segment],compareSegments:Array[Segment],compareSize:Int,location:Int, threshold:Int): Unit = {
     var blockSimilarThread = new BlockSimilarThread()
     blockSimilarThread.thread(segments,compareSegments,compareSize,location,threshold)
   }

  def main(args: Array[String]) {
    var blockSimilarThread = new BlockSimilarThread()
    val segment = new Segment(("14696602","120.152728,31.772667,120.15272350000001,31.772669,120.152719,31.772671,14604901,0"))
    val com = new Segment(("14696602","120.152728,31.772667,120.15272350000001,31.772669,120.152719,31.772771,14604901,0"))
    //println(blockSimilarThread.isCross(segment,com))
    val coordinate = new CoordinateUtil
    println(CoordinateUtil.calcABDistance(new GpsPair(120.152728,31.772667),new GpsPair(120.152719,31.772671)))
  }
}
