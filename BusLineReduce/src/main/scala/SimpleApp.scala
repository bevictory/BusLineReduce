package main.scala
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

import scala.collection.mutable.ArrayBuffer

/**
 * Created by dgh on 15-7-30.
 */
object SimpleApp {
  def main(args : Array[String]){
    /*val logFile = "/home/spark-1.4.1-bin-cdh4/README.md"
    val conf = new SparkConf().setAppName("Simple Application")
    val sc = new SparkContext(conf)
    val logData = sc.textFile(logFile)
    val numAs = logData.filter(line => line.contains("a")).count()
    val numBs = logData.filter(line => line.contains("b")).count()
    println("Lines with a: %s,Lines with b: %s".format(numAs,numBs))*/
    var segments = new Array[Array[String]](2)
    var segments1 = new ArrayBuffer[String]
    var segments2 = new ArrayBuffer[String]

    segments1+="hello"
    segments1+= "world"
    segments2+= "welcome"
    segments2 += "you"
    segments(0) = segments1.toArray
    segments(1) = segments2.toArray

    println(segments(1)(1))

  }

}
