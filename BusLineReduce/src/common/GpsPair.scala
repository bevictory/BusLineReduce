package common

import scala.collection.mutable.ArrayBuffer

/**
 * Created by dgh on 15-8-4.
 * GpsPair类
 */
class GpsPair(var longitude :Double , var latitude : Double) {
  var busNo:Int =0

  /**
   * 构造函数
   * @param lon 经度
   * @param lat 纬度
   */
  def this(lon:String,lat : String) {
    this(lon.toDouble,lat.toDouble)
  }
  def this() {
    this(0,0)
  }
}
object GpsPair {
  def main (args: Array[String]): Unit = {
    val gps = new GpsPair("100","123")
    //gps.latitude = 200
    println("hello"+gps.latitude)
    var segments = BusLine.splitIntoSegments(ArrayBuffer(new GpsPair(1,2),new GpsPair(3,4)),50)
    for (i <- 0 until segments.length) {
      println(segments(i))
    }
  }
}