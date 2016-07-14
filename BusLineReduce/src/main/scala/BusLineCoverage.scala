package main.scala


import java.sql.{DriverManager, Connection, PreparedStatement}
import java.util
import java.util.StringTokenizer
import java.util.logging.Logger

import common._
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path, FileSystem}
import org.apache.spark.rdd._
import org.apache.spark.{Logging, SparkContext, SparkConf}
import org.apache.velocity.runtime.log.Log
import Array._

import scala.collection.mutable.ArrayBuffer
/**
 * Created by dgh on 15-8-4.
 * 线网去重Spark方法
 */
object BusLineCoverage extends  Logging {
  var partitionNum:Int = 12
  /**
   * 分区切割的map函数
   * @param iteration 分区数据的迭代器容器
   * @tparam T
   * @return
   */
  def mapFunc[T](iteration: Iterator[T]): Iterator[(String, String)] = {
    var result = ArrayBuffer[(String, String)]()
    while (iteration.hasNext) {
      var res = ArrayBuffer[GpsPair]()
      var gps: GpsPair = new GpsPair()
      val str: T = iteration.next()
      val strToken = new StringTokenizer(str.toString)
      var busNo: String = null
      if (strToken.hasMoreTokens) {
        busNo = strToken.nextToken()
      }
      var flag: Int = 0
      if (strToken.hasMoreTokens) {
        gps.busNo = busNo.toInt
        gps.longitude = strToken.nextToken(",").trim().toDouble
        flag += 1
      }
      while (strToken.hasMoreTokens) {
        if (flag == 0) {
          gps = new GpsPair()
          gps.busNo = busNo.toInt
          gps.longitude = strToken.nextToken().toDouble
          flag += 1
        } else {
          gps.latitude = strToken.nextToken().toDouble
          res.+=(gps)
          flag = 0
        }
      }
      result.appendAll(BusLine.splitIntoSegments(res, 50,partitionNum))

    }
    result.toArray.iterator
  }

  /**
   * 将元组数组转化为Segment数组
   * @param array 元组数组
   * @return Segment类型的数组
   */
  def transformToSegment(array: Array[(String, String)]): Array[Segment] = {
    var segments = new Array[Segment](array.length)
    for (i <- 0 until array.length) {
      segments(i) = new Segment(array(i))
    }
    segments
  }

  /**
   * 将分区内元组数据分块保存Segment
   * @param array 元组数据
   * @return Segment类型数组的数组
   */
  def blockToSegment(array: Array[(String, String)]): Array[Array[Segment]] = {
    var blockSegments = new Array[Array[Segment]](2)
    var segment = new Segment(array(0))
    val blockNo = segment.blockNo

    var segments1 = new ArrayBuffer[Segment]()
    var segments2 = new ArrayBuffer[Segment]()
    for (i <- 0 until array.length) {
      segment = new Segment(array(i))
      if (segment.blockNo == blockNo) {
        segments1+= segment

      } else {
        segments2 += segment
      }

    }
    blockSegments(0) = segments1.toArray
    blockSegments(1) = segments2.toArray
    blockSegments
  }

  /**
   * 将Segment数组转化为元组数组
   * @param segments Segment数组
   * @return 元组数组
   */
  def transformToTuple(segments: Array[Segment]): Array[(String, String)] = {
    val array = new Array[(String, String)](segments.length)
    for (i <- 0 until segments.length) {
      array(i) = segments(i).segmentToTuple
    }
    array
  }

  /**
   * 分区去重函数
   * @param iteration 分区元组迭代器
   * @return 去重后的元组迭代器
   */
  def reduceFunc(iteration: Iterator[(String, String)]): Iterator[(String, String)] = {

    var prevBusNo = " "
    var array = iteration.toArray
    val listSize = array.length
    if (listSize > 0) prevBusNo = array(0)._1
    else {
      logInfo("!!!!!!!!!!!!!! no element in reduceFunc")
      return null
    }

    logInfo("*********** similar running")
    logInfo("*********** listSize" + listSize.toString)
    val threshold = 50
    var compareSize = 0
    var segments: Array[Segment] = transformToSegment(array)

    for (i <- 0 until listSize) {

      var segment = array(i)
      if (!segment._1.equals(prevBusNo)) {
        compareSize = i
        //if(compareSize > 10000) compareSize =10000
        prevBusNo = segment._1
        if (Segment.stringToSegment(array(i)).similarBusNo <= 1) {
          //array(i) = similar(array, compareSize, i, threshold)
          //logInfo("********* compareSize"+compareSize.toString)
          /*var j =0
          var compareSegment = Segment.stringToSegment(array(i))
          compareSegment.busNo = array(i)._1.toInt
          compareSegment.similarBusNo = 1
          while(j <=compareSize) {
            val segment = Segment.stringToSegment(array(j))
            if (compareSegment.isSimilarTo(segment, threshold)) {
              compareSegment.similarBusNo = segment.busNo


              j = compareSize
            }
            j+=1
          }
          array(i) = (compareSegment.busNo.toString, compareSegment.segmentToString)*/
          SimilarThread.similarNo = 1
          SimilarThread.thread(segments, compareSize, i, threshold)


        }
      }
      if (segment._1.equals(prevBusNo) && compareSize != i && compareSize > 0) {
        if (Segment.stringToSegment(array(i)).similarBusNo <= 1) {
          //logInfo("********* compareSize "+compareSize.toString)
          /*var j =0
          var compareSegment = Segment.stringToSegment(array(i))
          compareSegment.busNo = array(i)._1.toInt
          compareSegment.similarBusNo = 1
          while(j <=compareSize) {
            val segment = Segment.stringToSegment(array(j))
            if (compareSegment.isSimilarTo(segment, threshold)) {
              compareSegment.similarBusNo = segment.busNo
              j = compareSize
            }
            j+=1
          }
           array(i) = (compareSegment.busNo.toString, compareSegment.segmentToString)*/
          SimilarThread.similarNo = 1
          SimilarThread.thread(segments, compareSize, i, threshold)

        }
      }
    }
    //array(0) = (listSize.toString, array(0)._2)
    array = transformToTuple(segments)
    array.toIterator
  }

  /**
   * 查找比较范围：只和排序后前面的数据比较
   * @param segments 比较线段数组
   * @param preSize 之前比较大小，由于块内数据排序且只和排序后前面的数据比较去重，之前的比较范围肯定小于后面的比较范围
   * @param busNo 去重线段线路号
   * @return 比较大小
   */
  def compareSizeFind(segments: Array[Segment], preSize: Int, busNo: Int): Int = {
    for (i <- preSize until segments.length) {
      if (segments(i).busNo.toString > busNo.toString) return i
    }
    return segments.length
  }

  /**
   * 分区块间数据去重
   * @param iteration 分区元组数据迭代器容器
   * @return 块间去重后的迭代器容器
   */
  def reduce(iteration: Iterator[(String, String)]): Iterator[(String, String)] = {

    var array = iteration.toArray
    val listSize = array.length
    //var startTime = System.currentTimeMillis()

    var blockSegments = blockToSegment(array)
   // var endTime = System.currentTimeMillis()
   // logInfo("!!!!!!!!!blockToSegment" + ((endTime - startTime).toDouble / 1000).toString)
    if (listSize <= 0) {
      logInfo("!!!!!!!!!!!!!! no element in reduceFunc")
      return null
    }
    val segments1 = blockSegments(0)
    val segments2 = blockSegments(1)
    //logInfo("*********** similar running")
    //logInfo("*********** listSize"+listSize.toString)
    val threshold = 50
    var compareSize = 0
    for (i <- 0 to 1) {
      val blocksize = blockSegments(i).length
      compareSize = 0
      for (j <- 0 until blocksize) {
        if (blockSegments(i)(j).similarBusNo <= 1) {
          //startTime = System.currentTimeMillis()
          BlockSimilarThread.similarNo = 1
          val compare = if (i == 1) 0 else 1
          compareSize = compareSizeFind(blockSegments(compare), compareSize, blockSegments(i)(j).busNo)
          //logInfo("*********** block " +i.toString+ " comparesize " +compareSize.toString)
          if (compareSize > 0) BlockSimilarThread.thread(blockSegments(i), blockSegments(compare), compareSize, j, threshold)
          //logInfo("********** thread at " + compare.toString)
        //  endTime = System.currentTimeMillis()
          //logInfo("!!!!!!!!!compareTime" + ((endTime - startTime).toDouble / 1000).toString)
        }
      }
    }
    array = transformToTuple(concat(blockSegments(0), blockSegments(1)))
    array.toIterator

  }

  /**
   * 判断是否相似，已不再使用
   * @param array
   * @param compareSize
   * @param location
   * @param threshold
   * @return
   */
  def similar(array: Array[(String, String)], compareSize: Int, location: Int, threshold: Int): (String, String) = {
    var compareSegment = Segment.stringToSegment(array(location))
    compareSegment.busNo = array(location)._1.toInt

    compareSegment.similarBusNo = 1
    array(location) = compareSegment.segmentToTuple
    var i = 0
    while (i <= compareSize) {
      val segment = Segment.stringToSegment(array(i))
      if (compareSegment.isSimilarTo(segment, threshold)) {
        compareSegment.similarBusNo = segment.busNo
        array(location) = compareSegment.segmentToTuple
        i = compareSize
      }
      i += 1
    }
    array(location)
  }

  /**
   * 将String转化为元组
   * @param lines String
   * @return 元组
   */
  def calculate(lines: String): (String,String) = {
    var str = lines.substring(1, lines.length - 1)
    var strToken = new StringTokenizer(str, ",")
    var busNo: String = null
    if (strToken.hasMoreTokens) busNo = strToken.nextToken()
    var remains = str.substring(busNo.length, str.length)
    (busNo,remains)
    /*var segment = Segment.stringToSegment((busNo, remains))
    ()
    var length: Double = 0
    if (segment.similarBusNo == 0) {
      length = segment.calcABDistance()
    }
    logInfo("calculate " + length.toString)
    length*/
  }

  /**
   * 计算去重后各线段长度
   * @param iteration 迭代器
   * @return 距离数据迭代器
   */
  def calFunc(iteration: Iterator[(String,String)]): Iterator[Double] = {
    val str = iteration.toArray
    if (str.length <= 0) return null
    var array = new ArrayBuffer[Double]()
    val segments = transformToSegment(str)
    var i = 0
    //var j = 0
    while (i < str.length) {
      var length = 0.0
      if(segments(i).similarBusNo ==0) {
        length=segments(i).calcABDistance()
        array+=length
      }
      /*if (!length.equals(0.0)) {
        array += length
        j += 1
      }*/
      i += 1

    }

    array.toArray.toIterator
  }
  def MysqlDeal(): Unit ={
    Class.forName("com.mysql.jdbc.Driver")
    //classOf[com.mysql.jdbc.Driver]
    var conn:Connection= null
    var ps : PreparedStatement = null
    val isExist = "select count(*)  from information_SCHEMA.TABLES " +
      "where table_name = 'unisement_spark_test' and  TABLE_SCHEMA = 'czits_dev1'"
    val delete = "delete from unisegment_spark_test "
    conn = DriverManager.getConnection("jdbc:mysql://192.168.200.24:3306/czits_dev1","root","123456")
    try {
      ps = conn.prepareStatement(isExist)
      val existed = ps.executeQuery()

      if(existed.next()) {
        ps= conn.prepareStatement(delete)
        ps.executeUpdate()
      }

    }finally {
      conn.close()
    }

  }
  def sparkToDB(iteration:Iterator[(String,String)]):Unit ={
    //Class.forName("com.mysql.jdbc.Driver").newInstance()
    MysqlDeal()
    Class.forName("com.mysql.jdbc.Driver")
    //classOf[com.mysql.jdbc.Driver]
    var conn:Connection= null
    var ps : PreparedStatement = null
    val sql  = "insert into unisegment_spark_test(segmentno,startlong,startlat,midlong,midlat,endlong,endlat) " +
      "values(?,?,?,?,?,?,?)"
    conn = DriverManager.getConnection("jdbc:mysql://192.168.200.24:3306/czits_dev1" +
      "?useServerPrepStmts=false&rewriteBatchedStatements=true","root","123456")
    conn.setAutoCommit(false)

    var segments  = transformToSegment(iteration.toArray)
    var id =1
    //var sqls = new ArrayBuffer[String]()

    try {
      ps = conn.prepareStatement(sql)

      ps.clearBatch()
      for (i <- 0 until segments.length) {
        if (segments(i).similarBusNo == 0) {
          /*var insertSql:String =""
          insertSql =insertSql +"insert into unisegment_spark(id,segmentno,startlong,startlat,midlong,midlat,endlong,endlat) "+
          "values("+id+","+","+segments(i).busNo+","+segments(i).startLong+","+segments(i).startLat+","+segments(i).middleLong+","
          segments(i).middleLat+","+segments(i).endLong+","+segments(i).endLat+")"
          sqls+=insertSql*/

          //ps.setInt(1, id)
          ps.setInt(1, segments(i).busNo)
          ps.setDouble(2, segments(i).startLong)
          ps.setDouble(3, segments(i).startLat)
          ps.setDouble(4, segments(i).middleLong)
          ps.setDouble(5, segments(i).middleLat)
          ps.setDouble(6, segments(i).endLong)
          ps.setDouble(7, segments(i).endLat)
          ps.addBatch()
          //ps.executeUpdate()

          id += 1
        }
      }
      ps.executeBatch()
      conn.commit()
    }finally {
      ps.close()
      conn.close()
    }

  }

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("BusLinesCoverage Spark")
     conf.set("SplitMile","25")
     val sc = new SparkContext(conf)

    /*val hadoop_conf = new Configuration()
    val fileSystem = FileSystem.get(hadoop_conf)
    if(fileSystem.isDirectory(new Path("/user/daiguohui/Spark/BusLinesCoverage/result"))){
      logInfo("result exist")
      fileSystem.delete(new Path("/user/daiguohui/Spark/BusLinesCoverage/result"),false)
    }
    if(fileSystem.isDirectory(new Path("/user/daiguohui/Spark/BusLinesCoverage/output"))){
      logInfo("output exist")
      fileSystem.delete(new Path("/user/daiguohui/Spark/BusLinesCoverage/output"),false)
    }*/
     //if(args.length ==1 ) BusLineCoverage.partitionNum = args(0).toInt

     /*val distance = sc.textFile("hdfs://192.168.200.21:8020/user/daiguohui/Spark/BusLinesCoverage/output2")
     val result = distance.map(calculate).mapPartitions(calFunc)
       result.saveAsTextFile("hdfs://192.168.200.21:8020/user/daiguohui/Spark/BusLinesCoverage/result")*/
     val textFile = sc.parallelize(SparkMysql.readFromDB(),1)
   // SparkMysql.readFromDB()
   // val textFile = sc.textFile("/home/daiguohui/segment.txt")
    // val partitionNum = 12
     var par=textFile.mapPartitions(mapFunc).repartitionAndSortWithinPartitions(new RoundRobinPartitioner(partitionNum,0))


     for( ite <- 1 to (partitionNum*2-1) ) {
       if(ite==1) {
         par = par.mapPartitions(reduceFunc).repartitionAndSortWithinPartitions(new RoundRobinPartitioner(partitionNum, ite))
        // val res = par.mapPartitions(calFunc)
        // res.saveAsTextFile("hdfs://192.168.200.21:8020/user/daiguohui/Spark/BusLinesCoverage/output")
        // return
       }else {
         if(ite == (partitionNum*2-1)){
           par = par.mapPartitions(reduce).repartitionAndSortWithinPartitions(new RoundRobinPartitioner(1, ite))

           //result.reduce(_+_)

         }
         else par = par.mapPartitions(reduce).repartitionAndSortWithinPartitions(new RoundRobinPartitioner(partitionNum, ite))
       }
       if(ite ==  (partitionNum*2-1)) {
         var  result=par.mapPartitions(calFunc)
         //MysqlDeal()
         par.foreachPartition(sparkToDB)


         //par.saveAsTextFile("hdfs://192.168.200.21:8020/user/daiguohui/Spark/BusLinesCoverage/result")
         //result.saveAsTextFile("hdfs://192.168.200.21:8020/user/daiguohui/Spark/BusLinesCoverage/output")
       }
     }


    /* var segment1 = Segment.stringToSegment(("1","2,3,3,4,5,6,0"))
     var segment2 = Segment.stringToSegment(("2","2,3,3,4,5,6,0"))
     var segment3 = Segment.stringToSegment(("3","2,3,3,4,5,6,0"))
     var array = new Array[(String,String)](3)
     array(0) = (segment1.busNo.toString,segment1.segmentToString)
     array(1) = (segment2.busNo.toString,segment2.segmentToString)
     array(2) = (segment3.busNo.toString,segment3.segmentToString)
     array=reduceFunc(array.toIterator).toArray
     for (i <- 0 until array.length) {
       println(array(i)._2)
     }*/
    /*var str = Array("(14696602,120.152728,31.772667,120.15272350000001,31.772669,120.152719,31.772671,0,0)", "(14696602,120.152719,31.772671,120.1527155,31.772671,120.152712,31.772671,0,0)")
    var result = calFunc(str.iterator)
    var array = result.toArray
    println(array.length)*/
  }
}
