package common

/**
 * Created by dgh on 15-8-28.
 */
class LineCross {
  def isCross(segment:Segment,compareSegment:Segment):Double = {
    val direction_long = segment.endLong - segment.startLong
    val direction_lat = segment.endLat - segment.startLat
    val direction_long_com = compareSegment.endLong - compareSegment.startLong
    val direction_lat_com = compareSegment.endLat - compareSegment.startLat
    //val endDis = CoordinateUtil.calcABDistance(new GpsPair(segment.endLong,segment.endLat),new GpsPair(compareSegment.endLong,compareSegment.endLat))
    //val startDis =CoordinateUtil.calcABDistance(new GpsPair(segment.startLong,segment.startLat),new GpsPair(compareSegment.startLong,compareSegment.startLat))
    var flag =true
    var angle = 0.0
    if(direction_long == 0.0 && direction_long_com ==0.0){
      angle = 0.0
      //return true
    }
    val threshold = 40
    var isRightAngle =0
    var gradent = 0.0
    var gradent_com =0.0
    if(direction_long == 0.0 ) {
      gradent = Double.MaxValue
      isRightAngle =1
    }
    else gradent = direction_lat/direction_long
    if(direction_long_com ==0.0) {
      gradent_com = Double.MaxValue
      isRightAngle =2
    }
    else gradent_com = direction_lat_com/direction_long_com
    if(isRightAngle == 0) {
      angle = Math.toDegrees(Math.atan(Math.abs((gradent - gradent_com) / (1 + gradent * gradent_com))))
      if (Math.toDegrees(Math.atan(Math.abs((gradent - gradent_com) / (1 + gradent * gradent_com)))) > threshold) {
        flag = false
      } else flag = true
    }else {
      if(isRightAngle==1){
        angle = 90-(Math.toDegrees(Math.atan(Math.abs(gradent_com))))
        if(90-(Math.toDegrees(Math.atan(Math.abs(gradent_com)))) > threshold) {
          flag = false
        }else flag =true
      }
      if(isRightAngle == 2 ){
        angle = 90-(Math.toDegrees(Math.atan(Math.abs(gradent))))
        if( 90-(Math.toDegrees(Math.atan(Math.abs(gradent)))) > threshold) {
          flag = false
        }else flag =true

      }
    }

    /*if(Math.abs(endDis-startDis) > 15) {
      return false
    }else return true*/
    return angle

    /*var flag = false
    val pi =  3.14159265358979323846
    if(Math.abs(Math.abs(Math.atan(direction_lat/direction_long))/pi*180 -Math.abs(Math.atan(direction_lat_com/direction_long_com))/pi*180) < 30) {
      flag = true
    } else flag=false
    if(!flag) {


      val quadrant_ = quadrant(direction_long, direction_lat)
      val quadrant_com = quadrant(direction_long_com, direction_lat_com)
      if (quadrant_ <= 4) {
        if (quadrant_com <= 4) {
          if ((quadrant_ - quadrant_com) % 2 == 0) {
            return true
          } else return false
        } else {
          return true
        }

      } else {
        if (quadrant_com > 4) {
          if ((quadrant_ - quadrant_com) % 2 == 0) {
            return true
          } else return false
        } else return true
      }
    }else return flag*/

  }
  def compareNear(segments:Array[Segment],compareSegments:Array[Segment]):Boolean ={
    var result = false
    var flag  =0
    if(segments(0).isSimilarTo(compareSegments(0))&&segments(2).isSimilarTo(compareSegments(2)) ||
        segments(0).isSimilarTo(compareSegments(2))&& segments(2).isSimilarTo(compareSegments(0))) {
      val distance = CoordinateUtil.calcABDistance(segments(1),compareSegments(1))
      val sin = Math.sin(Math.PI*isCross(segments(1),compareSegments(1))/180)
      if(segments(0).isSimilarTo(compareSegments(0))&&segments(2).isSimilarTo(compareSegments(2))) {
        val distanceA=CoordinateUtil.calcABDistance(segments(0),compareSegments(0))
        val distanceB = CoordinateUtil.calcABDistance(segments(2),compareSegments(2))
        if(distanceA-distance > (segments(0).calcABDistance()+segments(1).calcABDistance())/2*sin || distanceB -distance >(segments(2).calcABDistance()+segments(1).calcABDistance())/2*sin){
          result = false
        }else result = true
      }
      if(segments(0).isSimilarTo(compareSegments(2))&&segments(2).isSimilarTo(compareSegments(0)) ) {
        val distanceA=CoordinateUtil.calcABDistance(segments(0),compareSegments(2))
        val distanceB = CoordinateUtil.calcABDistance(segments(2),compareSegments(0))
        if(distanceA-distance >(segments(0).calcABDistance()+segments(1).calcABDistance())/2*sin ||distanceB -distance >(segments(2).calcABDistance()+segments(1).calcABDistance())/2*sin){
          result = false
        }else result = true
      }

    }else {
      val segment:Segment = new Segment(new GpsPair(segments(0).startLong,segments(0).startLat),
        new GpsPair(segments(2).endLong,segments(2).endLat))
      val compareSegment:Segment = new Segment(new GpsPair(compareSegments(0).startLong,compareSegments(0).startLat),
        new GpsPair(compareSegments(2).endLong,compareSegments(2).endLat))
      val angle = isCross(segment,compareSegment)

       if(angle >20 ) {
        if((segment.calcABDistance()*Math.sin(Math.toRadians(angle)) < 50 ||
           CoordinateUtil.calcABDistance(segment,compareSegment) < 25) ){
          //if(segments(1).calcABDistance()*Math.sin(Math.toRadians(isCross(segments(1),compareSegments(1)))) < 50) {
            result = true
          //}else result =false

          /*if (isCross(compareSegments(1), segments(0)) > 20) {
            flag += 1
          }
          if (isCross(compareSegments(1), segments(2)) > 20) {
            flag += 2
          }
          if (flag > 0) {
            if (flag == 1) {
              if (isCross(segments(0), segments(1)) < 40||
                isCross(segments(0),segments(1)) >60 ) {
                result = false
              } else result = true

            }
            if (flag == 2) {
              if (isCross(segments(2), segments(1)) < 40||
                isCross(segments(2),segments(1)) >60 ) {
                result = false
              } else result = true
            }
            if (flag == 3) {
              if (isCross(segments(2), segments(1)) < 40 || isCross(segments(0), segments(1)) < 40||
                isCross(segments(2),segments(1)) >60 || isCross(segments(0),segments(1)) > 60) {
                result = false
              } else result = true
            }
          } else*/
        }
        else result = false
      }
      else result = true




      /*if (isCross(compareSegments(0), segments(1)) > 20) {
        flag += 1
      }
      if (isCross(compareSegments(2), segments(1)) > 20) {
        flag += 2
      }
      if (flag > 0) {
        if (flag == 1) {
          if (isCross(compareSegments(0), compareSegments(1)) < 40) {
            result = false
          } else result = true

        }
        if (flag == 2) {
          if (isCross(compareSegments(2), compareSegments(1)) < 40) {
            result = false
          } else result = true
        }
        if (flag == 3) {
          if (isCross(compareSegments(2), compareSegments(1)) < 40 || isCross(compareSegments(0), compareSegments(1)) < 40||
            isCross(compareSegments(2),compareSegments(1)) >60 || isCross(compareSegments(0),compareSegments(1)) > 60) {
            result = false
          } else result = true
        }
      } else result = false*/
    }

     return result

  }

  def isCross_(segments:Array[Segment],compareSegments:Array[Segment]):Boolean = {

    val segment = segments(1)
    val compareSegment = compareSegments(1)
    val angle  = isCross(segment,compareSegment)

    if (angle > 40 ){
      return false
    } else if (angle > 20) {
      return  compareNear(segments,compareSegments)
    }else return true
  }
  def quadrant(x:Double ,y:Double):Int ={
    if(x>0) {
      if(y>0) {
        return 1
      }else if(y==0) {
        return 5
      }else return 4
    }else if(x<0) {
      if(y>0){
        return 2
      }else if(y==0){
        return 7
      }else return 3
    }else {
      if(y>0) return 6
      else return 8
    }

  }

}
object  LineCross {
  def isCross(segments: Array[Segment],compareSegment:Array[Segment]) :Boolean ={
    val lineCross = new LineCross()
    lineCross.isCross_(segments,compareSegment)
  }
}
