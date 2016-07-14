package common

import java.util.StringTokenizer

/**
 * Created by dgh on 15-8-4.
 * Segment 类
 */

class Segment(var startLong:Double , var startLat:Double, var  middleLong:Double,
              var middleLat:Double,var endLong:Double,var endLat:Double,var similarBusNo:Integer = 0 ) {
  var busNo:Int =0
  var blockNo:Int = 0
  var start:GpsPair =null
  var middle:GpsPair = null
  var end:GpsPair =null
  def this() {
    this(0,0,0,0,0,0,0)
  }
  def this(startGps: GpsPair, middle:GpsPair,endGps: GpsPair) {
    this(startGps.longitude, startGps.latitude,middle.longitude,middle.latitude, endGps.longitude, endGps.latitude)
  }
  def this(startGps: GpsPair, endGps:GpsPair) {
    this()
    this.start = startGps
    this.end = endGps
    this.middle= getMiddle(startGps,endGps)
    this.startLong=startGps.longitude
    this.startLat = startGps.latitude
    this.middleLong = middle.longitude
    this.middleLat = middle.latitude
    this.endLong =endGps.longitude
    this.endLat = endGps.latitude
    this.similarBusNo =0

  }
  /**BusLineCoverage use*/
  /**
   * Segment 构造器，BusLineCoverage 使用
   * @param tuple 元组
   */
  def this(tuple:(String,String)){
    this()
    val strToken:StringTokenizer = new StringTokenizer(tuple._2)

    this.busNo = tuple._1.toInt
    //this.blockNo = (busNo.toString.hashCode()&Integer.MAX_VALUE)%(12*2)
    var i :Int= 1
    while(strToken.hasMoreTokens) {
      //if (i==0)  segment.busNo = strToken.nextToken().toInt
      if(i==1) {this.startLong = strToken.nextToken(",").toDouble}
      if(i==2) this.startLat = strToken.nextToken().toDouble
      if(i==3) this.middleLong = strToken.nextToken().toDouble
      if(i==4) this.middleLat = strToken.nextToken().toDouble
      if(i==5) this.endLong = strToken.nextToken().toDouble
      if(i==6) this.endLat = strToken.nextToken().toDouble
      if(i==7) this.similarBusNo = strToken.nextToken().toInt
      if(i==8) this.blockNo = strToken.nextToken().toInt
      i+=1
    }

  }
  /**BusLine.SplitToSegment use**/
  /**
   * Segment 构造器，BusLine.SplitToSegments使用
   * @param busNo 线路号
   * @param startGps 开始Gps
   * @param endGps 结束Gps
   */
  def this(busNo:Int,startGps:GpsPair,endGps:GpsPair,partitionNum:Int =12) {
    this(startGps,endGps  )
    this.busNo = busNo
    this.blockNo = (busNo.toString.hashCode()&Integer.MAX_VALUE)%(partitionNum*2)
  }

  /**
   * 获取中间Gps数据
   * @param startGps 开始Gps
   * @param endGps 结束Gps
   * @return 中间Gps数据
   */
  def getMiddle(startGps: GpsPair,endGps:GpsPair): GpsPair ={
    val gps =  new GpsPair(startGps.longitude + (endGps.longitude - startGps.longitude)
      * 0.5, startGps.latitude + (endGps.latitude - startGps.latitude) * 0.5)
    gps
  }
  def stringToSegment(tuple:(String,String)) : Segment={
    val strToken:StringTokenizer = new StringTokenizer(tuple._2)
    val segment:Segment = new Segment()
    segment.busNo = tuple._1.toInt
    var i :Int= 1
    while(strToken.hasMoreTokens) {
      //if (i==0)  segment.busNo = strToken.nextToken().toInt
      if(i==1) {segment.startLong = strToken.nextToken(",").toDouble}
      if(i==2) segment.startLat = strToken.nextToken().toDouble
      if(i==3) segment.middleLong = strToken.nextToken().toDouble
      if(i==4) segment.middleLat = strToken.nextToken().toDouble
      if(i==5)segment.endLong = strToken.nextToken().toDouble
      if(i==6) segment.endLat = strToken.nextToken().toDouble
      if(i==7) segment.similarBusNo = strToken.nextToken().toInt
      if(i==8) segment.blockNo = strToken.nextToken().toInt
      i+=1
    }
    segment
  }

  /**
   * Segment转为元组
   * @return 元组
   */
  def segmentToTuple:(String,String) ={
    val str1 = this.busNo.toString
    var str2 =new String()
    str2 = str2 +this.startLong +"," +this.startLat+","+this.middleLong+","+this.middleLat+","+this.endLong+","+this.endLat+","+this.similarBusNo+","+this.blockNo
    (str1,str2)
  }



  /**
   * Segment比较
   * @param compare 比较的segment
   * @param threshold 阀值
   * @return ture
   */
  def isSimilarTo(compare:Segment,threshold:Double = 50) :Boolean ={
    if(CoordinateUtil.calcABDistance(new GpsPair(this.middleLong,this.middleLat),new GpsPair(compare.middleLong,compare.middleLat)) > threshold) false else true
  }

  /**
   * 计算不同Gps的距离
   * @return 距离
   */
  def calcABDistance():Double = {
    CoordinateUtil.calcABDistance(new GpsPair(this.startLong,this.startLat),new GpsPair(this.endLong,this.endLat))
  }


}
object Segment {
  def stringToSegment(tuple:(String,String)) :Segment ={
    val segment = new Segment()
    val s = segment.stringToSegment(tuple)
    s
  }
  val segment = new Segment()

   def main (args: Array[String]): Unit = {
      val segment = new Segment(113.380301,22.514519,0,0,113.380301,22.511962)
     println(segment.calcABDistance())

  }
}