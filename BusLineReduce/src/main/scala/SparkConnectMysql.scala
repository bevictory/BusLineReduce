package main.scala

import java.io._
import java.sql.{SQLException, PreparedStatement, Connection, DriverManager}
import java.util.StringTokenizer

import scala.collection.mutable.ArrayBuffer

/**
 * Created by dgh on 15-8-21.
 */
object SparkConnectMysql {
  def mapFunction(iteration: Iterator[(String , String)]):Unit = {
     var conn:Connection = null
     var ps : PreparedStatement = null
    val sql  = "insert into unisegment_spark(id,segmentno,stratlong,startlat,midlong,midlat,endlong,endlat) " +
      "values(?,?,?,?,?,?,?,?)"
    conn = DriverManager.getConnection("jdbc:mysql://192.168.200.24/czits_test","root","123456")
    var id =1
    while(iteration.hasNext) {
      var segmentno = iteration.next()._1
      var str = iteration.next()._2
      var startlong:Double =0
      var startlat:Double =0
      var midlong:Double =0
      var midlat :Double  =0
      var endlong:Double =0
      var endlat: Double =0
      var strToken = new StringTokenizer(str,",")
      var i =1
      while(strToken.hasMoreTokens)
        if(i==1) startlong = strToken.nextToken().toDouble
        if(i==2) startlat = strToken.nextToken().toDouble
        if(i==3) midlong = strToken.nextToken().toDouble
        if(i==4) midlat = strToken.nextToken().toDouble
        if(i==5) endlong = strToken.nextToken().toDouble
        if(i==6) endlat = strToken.nextToken().toDouble
      }
      id +=1
    }
  def readFromDB():Unit={
    var conn:Connection = null
    var ps:PreparedStatement = null
    Class.forName("com.mysql.jdbc.Driver")
    val jdbc = "jdbc:mysql://192.168.200.24:3306/czits_dev1"
    val username ="root"
    val password ="123456"
    conn = DriverManager.getConnection(jdbc,username,password)
    var sql ="select segmentno,JWD from segment"
    var segments = new ArrayBuffer[String]()
    try {
      ps = conn.prepareStatement(sql)
      var result = ps.executeQuery()

      while(result.next()) {
        var str:String = new String()
        str += result.getString("segmentno")
        str += "     "
        str += result.getString("JWD")
        println(str)
        segments+= str
      }

    }finally {
      conn.close()
    }
    val file = new File("/home/dgh/segment.txt")
    if(file.exists()) {
      file.delete()
    } else {
      file.createNewFile()
    }
    val dataSave= segments.toArray
    var out  = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))

    for(i <- 0 until dataSave.size) {
      out.println(dataSave(i))

    }
    out.close()
  }
  def MysqlDeal(): Unit ={
    Class.forName("com.mysql.jdbc.Driver")
    //classOf[com.mysql.jdbc.Driver]
    var conn:Connection= null
    var ps : PreparedStatement = null
    val isExist = "select count(*)  from information_SCHEMA.TABLES " +
      "where table_name = 'unisement_spark' and  TABLE_SCHEMA = 'czits_dev1'"
    val delete = "delete from unisegment_spark "
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
  def main(args: Array[String]) {
    MysqlDeal()
  }
}
