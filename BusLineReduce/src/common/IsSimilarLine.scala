package common

/**
 * Created by dgh on 15-9-2.
 */
class IsSimilarLine(var lineWidth:Double= 15,var busWidth:Double = 12){
  /**
   * 求两个线段的夹角
   * @param segment
   * @param compareSegment
   * @return 夹角角度
   */
  def angleBtwSegment(segment:Segment,compareSegment:Segment) :Double ={
    val direction_long = segment.endLong - segment.startLong
    val direction_lat = segment.endLat - segment.startLat
    val direction_long_com = compareSegment.endLong - compareSegment.startLong
    val direction_lat_com = compareSegment.endLat - compareSegment.startLat
    var flag =true
    var angle = 0.0
    if(direction_long == 0.0 && direction_long_com ==0.0){
      angle = 0.0
    }
    val threshold = 40
    var isRightAngle =0
    var gradent = 0.0
    var gradent_com =0.0
    if(direction_long == 0.0 ) {
      gradent = Double.MaxValue
      isRightAngle =1
    } else gradent = direction_lat/direction_long
    if(direction_long_com ==0.0) {
      gradent_com = Double.MaxValue
      isRightAngle =2
    } else gradent_com = direction_lat_com/direction_long_com
    if(isRightAngle == 0) {
      angle = Math.toDegrees(Math.atan(Math.abs((gradent - gradent_com) / (1 + gradent * gradent_com))))
    }else {
      if(isRightAngle==1){
        angle = 90-(Math.toDegrees(Math.atan(Math.abs(gradent_com))))
      }
      if(isRightAngle == 2 ){
        angle = 90-(Math.toDegrees(Math.atan(Math.abs(gradent))))
      }
    }
    return angle
  }
  def maxIntersectionAngle(segment:Segment) :Double ={
    val angle = Math.toDegrees(Math.asin(lineWidth/segment.calcABDistance()))
    angle
  }
  def distanceBtwSegment(segment:Segment,compareSegment:Segment):Double ={
    val angle = angleBtwSegment(segment,compareSegment)
    val dis = compareSegment.calcABDistance()*Math.sin(Math.toRadians(angle))
    dis
  }
  def isInSameLine(segments:Array[Segment]):Boolean ={
    val segment:Segment = new Segment(new GpsPair(segments(0).startLong,segments(0).startLat),
      new GpsPair(segments(2).endLong,segments(2).endLat))
    if(distanceBtwSegment(segment,segments(0)) >lineWidth ||distanceBtwSegment(segment,segments(2)) >lineWidth) {
      return false
    }else return true
  }
  def isSimilarNear(segments:Array[Segment],compareSegments:Array[Segment]):Boolean ={
    if(segments(0).isSimilarTo(compareSegments(0))&&segments(2).isSimilarTo(compareSegments(2)) ||
      segments(2).isSimilarTo(compareSegments(0))&&segments(0).isSimilarTo((compareSegments(2)))) {
      return true
    }else return false

  }
  def isSimilar(segments:Array[Segment],compareSegments:Array[Segment]) :Boolean ={
    var result = true
    if(segments(1).calcABDistance() < busWidth) {
      result = true
    }else {
      val segment:Segment = new Segment(new GpsPair(segments(0).startLong,segments(0).startLat),
        new GpsPair(segments(2).endLong,segments(2).endLat))
      val compareSegment:Segment = new Segment(new GpsPair(compareSegments(0).startLong,compareSegments(0).startLat),
        new GpsPair(compareSegments(2).endLong,compareSegments(2).endLat))
      val angle = angleBtwSegment(segments(1),compareSegments(1))
      val threshold = (maxIntersectionAngle(segments(1))+maxIntersectionAngle(compareSegments(1)))
      if(angle >=threshold) {
        result = false
      } else if(angle  >= maxIntersectionAngle(segments(1))){
        if(isSimilarNear(segments,compareSegments)) {
          result = true
        }else {
          val segmentInSameLine = isInSameLine(segments)
          val compareSegmentInSameLine = isInSameLine(compareSegments)
          if(compareSegmentInSameLine && !segmentInSameLine) {
            if(angleBtwSegment(segments(1),compareSegment) >=
              (maxIntersectionAngle(segments(1))+maxIntersectionAngle(compareSegment))) {
              result = false
            }else {
              val angle_three = angleBtwSegment(segment,compareSegment)
              if(angle_three >= (maxIntersectionAngle(segment)+maxIntersectionAngle(compareSegment))){
                result = false
              }else result = true
            }
          }
          if(segmentInSameLine && !compareSegmentInSameLine) {
            if(angleBtwSegment(compareSegments(1),segment) >= maxIntersectionAngle(compareSegments(1))+maxIntersectionAngle(segment)) {
              result = false
            }else result = true
          }
          if(!segmentInSameLine && !compareSegmentInSameLine) {
            if(distanceBtwSegment(segments(1),compareSegments(1)) >lineWidth*2 ||
              distanceBtwSegment(segments(1),compareSegments(1)) >lineWidth*2) {
              result = true
            }else result = false
          }
          if(segmentInSameLine && compareSegmentInSameLine) {
            if(angleBtwSegment(segment,compareSegment) >=
              maxIntersectionAngle(segment)+maxIntersectionAngle(compareSegment)) {
              result = false
            }else  result = true
          }
        }
      }else result = true

    }
    return result
  }

}
object  IsSimilarLine {
  def isSimilar(segments: Array[Segment],compareSegments:Array[Segment]) :Boolean ={
    val similar = new IsSimilarLine()
    similar.isSimilar(segments ,compareSegments)
  }
}
