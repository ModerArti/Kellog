package ru.tversu

import scala.collection.mutable.ArrayBuffer

object Kellog {

  // параметры задачи
  private val a = 0
  private val b = 1
  private val n = 1000
  private val N = 10

  private val delta = (b - a).toDouble / n

  // массив Y
  var Y: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer.fill[Double](N, n + 1)(0)

  def main(args: Array[String]): Unit = {
    // заполнение первой строки массива единицами
    for (j <- 0 to n) {
      Y(0)(j) = 1
    }

    // вычисление интегралов для массива Y
    calculateYIntegral()

    // вычисление норм и вывод их на экран
    val normArray = calculateNorms()
    normArray.zipWithIndex.foreach {
      case (norm, index) => println(s"норма $index: $norm")
    }

    // вычисление мю и вывод их на экран
    val muArray = calculateMus(normArray)
    muArray.zipWithIndex.foreach {
      case (mu, index) => println(s"мю $index: $mu")
    }

    // вычисление массива Z
    val Z = calculateZ(normArray)

    // вывод на экран значения из массивов Z с чётными номерами
    println("Z0; Z2; Z4; Z6; Z8")
    for (j <- 0 to n) {
      print(s"${a + delta * j};")
      for (i <- 0 until N by 2) {
        print(s"${Z(i)(j)};")
      }
      println
    }

    // вывод на экран значения из массивов Z с нечётными номерами
    println("Z1;Z3;Z5;Z7;Z9")
    for (j <- 0 to n) {
      print(s"${a + delta * j};")
      for (i <- 1 until N by 2) {
        print(s"${Z(i)(j)};")
      }
      println
    }

    // Z "с крышечкой" и Z "с двумя крышечками"
    val z_first = Z(8)
    val z_second = Z(9)

    // вычисление итогового решения Z и лямбды и их вывод на экран
    val (resultZ, lambda) = calculateResult(z_first, z_second, muArray)
    println("Z result")
    for (j <- 0 to n) {
        println(s"${a + delta * j};${resultZ(j)};")
    }
    println(s"лямбда: $lambda")
  }

  // функция вычисления интеграла для каждой ячейки массива Y
  private def calculateYIntegral(): Unit = {
    for (i <- 1 until N) {
      for (j <- 0 to n) {
        Y(i)(j) = y_integral(i - 1, j)
      }
    }
  }

  // функция вычисления нормы для каждой ячейки массива норм
  private def calculateNorms(): ArrayBuffer[Double] = {
    val normArray = ArrayBuffer.fill[Double](N)(0)
    for (i <- 0 until N) {
      normArray(i) = norm_integral(i)
    }
    normArray
  }

  // функция вычисления значений массива мю
  private def calculateMus(normArray: ArrayBuffer[Double]): ArrayBuffer[Double] = {
    val muArray = ArrayBuffer.fill[Double](N)(0)
    for (i <- 1 until N) {
      muArray(i) = normArray(i - 1) / normArray(i)
    }
    muArray
  }

  // функция вычисления значений в массиве Z
  private def calculateZ(normArray: ArrayBuffer[Double]): ArrayBuffer[ArrayBuffer[Double]] = {
    val Z = Y
    for (i <- 0 until N) {
      for (j <- 0 to n) {
        Z(i)(j) /= normArray(i)
      }
    }
    Z
  }

  // функция вычисления результата (Z результирующий и лямбда)
  private def calculateResult(
                                z_first: ArrayBuffer[Double],
                                z_second: ArrayBuffer[Double],
                                muArray: ArrayBuffer[Double]
                              ): (ArrayBuffer[Double], Double) = {
    var isTheSame = true
    for (j <- 0 until n) {
      if (math.abs(z_first(j) - z_second(j)) >= 0.0001) {
        isTheSame = false
      }
    }

    if (isTheSame) (z_first, muArray(9))
    else {
      val z = z_first
      for (j <- 0 until n)
        z(j) -= z_second(j)
      (z, -muArray(9))
    }
  }

  // функция взятия интеграла для вычисления нормы
  private def norm_integral(N: Int): Double = {
    val y = Y(N)
    var sum: Double = 0
    for (i <- 0 to n) {
      sum += y(i) * y(i) * delta
    }
    math.sqrt(sum)
  }

  // функция взятия интеграла для вычисления значений в массиве Y
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
