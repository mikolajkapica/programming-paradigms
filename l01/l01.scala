def reverse4(x1: Int, x2: Int, x3: Int, x4: Int): (Int, Int, Int, Int) = (x4, x3, x2, x1)

def reverse4_tests =
    val test1 = reverse4(1, 2, 3, 4) == (4, 3, 2, 1)
    val test2 = reverse4(1, 2, 3, 4) != (1, 2, 3, 4)
    test1 && test2

def sumProd(s: Int, e: Int): (Int, Int) = {
  if (s > e) throw new IllegalArgumentException("s > e")
  if (e == 0) return (0, 0)
  def aux(current: Int, sum: Int, prod: Int): (Int, Int) = {
    if (current > e) (sum, prod)
    else aux(current + 1, sum + current, prod * current)
  }
  aux(s, 0, 1)
}

def sumProd_tests =
    val test1 = sumProd(1, 5) == (15, 120)
    val test2 = sumProd(1, 1) == (1, 1)
    val test3 = sumProd(1, 10) == (55, 3628800)
    val test4 = sumProd(-4, 10) == (-4+(-3)+(-2)+(-1)+0+1+2+3+4+5+6+7+8+9+10, 0)
    val test5 = sumProd(3,3) == (3,3)
    val test6 = sumProd(0, 0) == (0, 0)
    test1 && test2 && test3

def isPerfect(n: Int): Boolean = {
    if (n < 1) return false
    val divisors = {
        def aux(current: Int, divisors: List[Int]): List[Int] = {
            if (current == 0) divisors
            else if (n % current == 0) aux(current - 1, current :: divisors)
            else aux(current - 1, divisors)
        }
        aux(n-1, List())
    }
    val divisors_sum = divisors.foldLeft(0)(_ + _)
    divisors_sum == n
}

def isPerfect_tests =
    val test1 = isPerfect(6) == true
    val test2 = isPerfect(28) == true
    val test3 = isPerfect(496) == true
    val test4 = isPerfect(7) == false
    val test5 = isPerfect(12) == false
    test1 && test2 && test3 && test4 && test5

def insert[A](lst: List[A], x: A, pos: Int): List[A] = {
    def aux(lst: List[A], current: Int): List[A] =
        lst match {
            case Nil => List(x)
            case h :: t => if (current == pos) x :: lst else h :: aux(t, current + 1)
        }
    if pos < 0 then x :: lst 
    else aux(lst, 0)
}

def insert_tests = {
    val test1 = insert(List(1, 2, 3, 4), 0, 2) == List(1, 2, 0, 3, 4)
    val test2 = insert(List(1, 2, 3, 4), 0, 0) == List(0, 1, 2, 3, 4)
    val test3 = insert(List(1, 2, 3, 4), 0, 4) == List(1, 2, 3, 4, 0)
    val test4 = insert(List(1, 2, 3, 4), 0, 5) == List(1, 2, 3, 4, 0)
    val test5 = insert(List(1, 2, 3, 4), 0, -1) == List(0, 1, 2, 3, 4)
    test1 && test2 && test3 && test4 && test5
}


object Main {
    def main(args: Array[String]): Unit = {
        println("reverse4_tests: " + reverse4_tests)
        println("sumProd_tests: " + sumProd_tests)
        println("isPerfect_tests: " + isPerfect_tests)
        println("insert_tests: " + insert_tests)
    }
}
