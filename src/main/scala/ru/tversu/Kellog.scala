package ru.tversu

import scala.collection.mutable.ArrayBuffer

object Kellog {

  private val a = 0
  private val b = 1
  private val n = 1000
  private val N = 10

  private val delta = (b - a).toDouble / n

  var Y: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer.fill[Double](N, n + 1)(0)

  def main(args: Array[String]): Unit = {
    for (j <- 0 to n) {
      Y(0)(j) = 1
    }

    calculateYIntegral()

    val normArray = calculateNorms()
    normArray.zipWithIndex.foreach {
      case (norm, index) => println(s"норма $index: $norm")
    }

    val muArray = calculateMus(normArray)
    muArray.zipWithIndex.foreach {
      case (mu, index) => println(s"мю $index: $mu")
    }

    val Z = calculateZ(normArray)

    println("Z0; Z2; Z4; Z6; Z8")
    for (j <- 0 to n) {
      print(s"${a + delta * j};")
      for (i <- 0 until N by 2) {
        print(s"${Z(i)(j)};")
      }
      println
    }

    println("Z1;Z3;Z5;Z7;Z9")
    for (j <- 0 to n) {
      print(s"${a + delta * j};")
      for (i <- 1 until N by 2) {
        print(s"${Z(i)(j)};")
      }
      println
    }

    val z_first = Z(8)
    val z_second = Z(9)

    val resultZ = calculateResultZ(z_first, z_second)
    println("Z result")
    for (j <- 0 to n) {
        println(s"${a + delta * j};${resultZ(j)};")
    }

    val lambda = muArray(9)
    println(s"лямбда: $lambda")

    println("")

  }

  private def calculateYIntegral(): Unit = {
    for (i <- 1 until N) {
      for (j <- 0 to n) {
        Y(i)(j) = y_integral(i - 1, j)
      }
    }
  }

  private def calculateNorms(): ArrayBuffer[Double] = {
    val normArray = ArrayBuffer.fill[Double](N)(0)
    for (i <- 0 until N) {
      normArray(i) = norm_integral(i)
    }
    normArray
  }

  private def calculateMus(normArray: ArrayBuffer[Double]): ArrayBuffer[Double] = {
    val muArray = ArrayBuffer.fill[Double](N)(0)
    for (i <- 1 until N) {
      muArray(i) = normArray(i - 1) / normArray(i)
    }
    muArray
  }

  private def calculateZ(normArray: ArrayBuffer[Double]): ArrayBuffer[ArrayBuffer[Double]] = {
    val Z = Y
    for (i <- 0 until N) {
      for (j <- 0 to n) {
        Z(i)(j) /= normArray(i)
      }
    }
    Z
  }

  private def calculateResultZ(
                                z_first: ArrayBuffer[Double],
                                z_second: ArrayBuffer[Double]
                              ): ArrayBuffer[Double] = {
    var isTheSame = true
    for (j <- 0 until n) {
      if (math.abs(z_first(j) - z_second(j)) >= 0.0001) {
        isTheSame = false
      }
    }

    if (isTheSame) z_first
    else {
      val z = z_first
      for (j <- 0 until n)
        z(j) -= z_second(j)
      z
    }
  }

  private def norm_integral(N: Int): Double = {
    val y = Y(N)
    var sum: Double = 0
    for (i <- 0 to n) {
      sum += y(i) * y(i) * delta
    }
    math.sqrt(sum)
  }

  private def y_integral(N: Int, k: Int): Double = {
    val y = Y(N)
    val x_k = a + delta * k
    val kernel = (x: Double, t: Double) =>
      math.sqrt(x * x + t * t)

    var sum: Double = 0
    for (l <- 0 to n) {
      val t_l = a + delta * l
      sum += kernel(x_k, t_l) * y(l) * delta
    }
    sum
  }

}
