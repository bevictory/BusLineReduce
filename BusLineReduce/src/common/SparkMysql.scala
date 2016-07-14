package common

import java.io.{FileOutputStream, BufferedOutputStream, PrintWriter, File}
import java.sql.{DriverManager, PreparedStatement, Connection}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by dgh on 15-8-25.
 */
class SparkMysql {
  def readFromDB():Array[String]={
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
        //println(str)
        segments+= str
      }

    }finally {
      ps.close()
      conn.close()
    }
    segments.toArray
    /*val file = new File("/home/daiguohui/segment.txt")
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
    out.close()*/
  }


}
object SparkMysql {
  def readFromDB():Array[String] =
  {
    val SparkMysql = new SparkMysql()
    val result = SparkMysql.readFromDB()
    result
  }

  def main(args: Array[String]) {
    readFromDB()
  }
}
