def fib_imp(n: Int): Int = {
  if (n == 1 || n == 2) 1
  else {
    var a = 1
    var b = 1
    var c = 0
    for (i <- 3 to n) {
      c = a + b
      a = b
      b = c
    }
    c
  }
}

def fib_triangle(row: Int): Array[Int] = {
  val triangle = Array.ofDim[Int](row, row)
  for (i <- 0 until row) {
    triangle(i)(0) = 1
    triangle(i)(i) = 1
  }
  for (i <- 2 until row) {
    for (j <- 1 until i) {
      triangle(i)(j) = triangle(i-1)(j-1) + triangle(i-1)(j)
    }
  }
  triangle(row-1)
}

def power(n: Int): Int = {
  var res = 1
  for (i <- 2 to n) {
    res *= i
  }
  res
}

def binomial_coefficient(n: Int, k: Int): Int = {
  power(n) / (power(k) * power(n-k))
}

def skip_imp(m: Int, n: Int): Int = {
  if (m == 1) fib_imp(n)
  else {
    var sum = 0
    // var coeffs = fib_triangle(m)
    for (i <- 0 to m - 1) {
        // sum += coeffs(i) * fib_imp(i + n)
        sum += binomial_coefficient(m-1, i) * fib_imp(i + n)
    }
    sum
  }
}

def tests_b(): Unit = {
  println("skip_imp (2, 2) = " + skip_imp(2, 2))
  println("skip_imp (5, 3) = " + skip_imp(5, 3))
  println("skip_imp (12, 4) = " + skip_imp(12, 4))
}

def main(): Unit = {
    tests_b()
}

