package common

import scala.collection.mutable.ArrayBuffer

/**
 * Created by dgh on 15-8-4.
 * Gps数据切割
 */
class BusLine {
  /**
   * 将Gps数据按阀值切割成元组数据保存
   * @param originalGpsPairs 原始的Gps数据
   * @param mile 阀值
   * @return 元组数组
   */
  def splitIntoSegments(originalGpsPairs:ArrayBuffer[GpsPair],mile:Int,partitionNum:Int =12) :ArrayBuffer[(String,String)] ={

    var segments = new ArrayBuffer[(String,String)]()
    for(i <- 0 until (originalGpsPairs.length-1) if(originalGpsPairs(i).busNo == originalGpsPairs(i+1).busNo )) {

      val dis = CoordinateUtil.calcABDistance(originalGpsPairs(i),originalGpsPairs(i+1))
      val splitNum = (dis/mile).toInt +1
      val increLong = ((originalGpsPairs(i+1).longitude-originalGpsPairs(i).longitude)/splitNum)
      val increLat = ((originalGpsPairs(i+1).latitude-originalGpsPairs(i).latitude)/splitNum)
      val busNo = originalGpsPairs(i).busNo
      for( j <- 1 until  splitNum ) {

        var segment = new Segment(busNo,new GpsPair(originalGpsPairs(i).longitude+increLong*(j-1),originalGpsPairs(i).latitude+
        increLat*(j-1)),new GpsPair(originalGpsPairs(i).longitude+increLong*(j),originalGpsPairs(i).latitude+
          increLat*(j)),partitionNum).segmentToTuple
        segments+= segment
      }
      var segment = (new Segment(busNo,new GpsPair(originalGpsPairs(i).longitude+increLong*(splitNum-1),originalGpsPairs(i).latitude+
        increLat*(splitNum-1)),originalGpsPairs(i+1),partitionNum)).segmentToTuple
      segments += segment
    }
    segments
  }

}
object BusLine {
  val busLine = new BusLine()
  def splitIntoSegments(originalGps:ArrayBuffer[GpsPair],mile:Int,partitionNum:Int =12):ArrayBuffer[(String,String)] ={
    busLine.splitIntoSegments(originalGps,mile,partitionNum)
  }
  def main (args: Array[String]) {
    var segments = BusLine.splitIntoSegments(ArrayBuffer(new GpsPair(1,2),new GpsPair(3,4)),50)
    for (i <- 0 until segments.length) {
      println(segments(i)._1,segments(i)._2)
    }
  }
}
