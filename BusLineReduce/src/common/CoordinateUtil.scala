package common

/**
 * Created by dgh on 15-8-4.
 * 计算Gps数据点的距离
 */
class CoordinateUtil {
  val EARTH_RADIUS :Int = 6378137

  /**
   * 计算两个Gps点的距离
   * @param lng1 经度
   * @param lat1 纬度
   * @param lng2 经度
   * @param lat2 纬度
   * @return 距离
   */
  def calcABDistance(lng1:Double,lat1:Double,lng2:Double,lat2:Double): Double= {
    if(Math.abs(lng1-lng2) < 0.000001 && Math.abs(lat1-lat2) < 0.000001) return 0
    var distance:Double = 0
    distance = EARTH_RADIUS *Math.acos(Math.sin(Math.toRadians(lat1))*Math.sin(Math.toRadians(lat2))
      +Math.cos(Math.toRadians(lat1))*Math.cos(Math.toRadians(lat2))
    *Math.cos(Math.toRadians(lng2-lng1)))
    //println(lng1,lat1,lng2,lat2,distance)
    distance

  }

  /**
   * 计算两Gps数据点距离
   * @param a Gps
   * @param b Gps
   * @return 距离
   */
  def calcABDistance(a:GpsPair,b:GpsPair) : Double ={
    if(a==null && b==null) {
      return -1
    }
    calcABDistance(a.longitude,a.latitude,b.longitude,b.latitude)
  }
  def calcABDistance(segment:Segment,compareSegment:Segment) :Double ={
    return calcABDistance(new GpsPair(segment.middleLong,segment.middleLat),new GpsPair(compareSegment.middleLong,compareSegment.middleLat))
  }
}
object CoordinateUtil {
  val coordinate = new CoordinateUtil
  def calcABDistance(a:GpsPair,b:GpsPair):Double = {
    return coordinate.calcABDistance(a,b)
  }

  def calcABDistance(segment: Segment,compareSegment:Segment) :Double ={
    return coordinate.calcABDistance(segment,compareSegment)
  }

  def main(args: Array[String]) :Unit ={
    println(CoordinateUtil.calcABDistance(new GpsPair(119.964741,31.78562),new GpsPair(119.964741,31.78672)))
  }
}