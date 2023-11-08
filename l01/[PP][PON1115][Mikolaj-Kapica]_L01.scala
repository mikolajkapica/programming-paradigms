def reverse4[A](x1: A, x2: A, x3: A, x4: A): (A, A, A, A) = (x4, x3, x2, x1)

def reverse4_tests =
    val test1 = reverse4(1, 2, 3, 4) == (4, 3, 2, 1)
    val test2 = reverse4(1, 2, 3, 4) != (1, 2, 3, 4)
    val test3 = reverse4(0, 0, 0, 0) == (0, 0, 0, 0)
    val test4 = reverse4(-1, -2, -3, -4) == (-4, -3, -2, -1)
    test1 && test2 && test3 && test4

def sumProd(s: Int, e: Int): (Int, Int) = {
  if (s >= e) throw new IllegalArgumentException("s > e")
  def aux(current: Int, sum: Int, prod: Int): (Int, Int) = {
    if (current >= e) (sum, prod)
    else aux(current + 1, sum + current, prod * current)
  }
  aux(s, 0, 1)
}

def sumProd_tests =
    val test1 = sumProd(1, 5) == (1+2+3+4, 1*2*3*4)
    val test2 = sumProd(1, 2) == (1, 1)
    val test3 = sumProd(1, 10) == (1+2+3+4+5+6+7+8+9, 1*2*3*4*5*6*7*8*9)
    val test4 = sumProd(-4, 10) == (-4+(-3)+(-2)+(-1)+0+1+2+3+4+5+6+7+8+9, (-4)*(-3)*(-2)*(-1)*0*1*2*3*4*5*6*7*8*9)
    val test5 = sumProd(-4, -1) == (-4+(-3)+(-2), (-4)*(-3)*(-2))
    test1 && test2 && test3 && test4 && test5

def isPerfect(n: Int): Boolean = {
    if (n < 1) throw new IllegalArgumentException("n must be natural (n > 0)")
    val divisorsSum = {
        def aux(current: Int, divisorsSum: Int): Int = {
            if (current == 0) divisorsSum 
            else if (n % current == 0) aux(current - 1, current + divisorsSum)
            else aux(current - 1, divisorsSum)
        }
        aux(n / 2, 0)
    }
    divisorsSum == n
}

def isPerfect_tests =
    val test1 = isPerfect(6) == true
    val test2 = isPerfect(28) == true
    val test3 = isPerfect(496) == true
    val test4 = isPerfect(7) == false
    test1 && test2 && test3 && test4

def insert[A](lst: List[A], x: A, pos: Int): List[A] = {
    def aux(lst: List[A], current: Int): List[A] =
        if lst == Nil then List(x)
        else if (current == pos) then x :: lst
        else lst.head :: aux(lst.tail, current + 1)
    if pos < 0 then x :: lst 
    else aux(lst, 0)
}

def insert_tests = {
    val test1 = insert(List(1, 2, 3, 4), 0, 2) == List(1, 2, 0, 3, 4)
    val test2 = insert(List(1, 2, 3, 4), 0, 0) == List(0, 1, 2, 3, 4)
    val test3 = insert(List(), 0, 4) == List(0)
    val test4 = insert(List(1), 0, 5) == List(1, 0)
    val test5 = insert(List(1, -2, 3, 4), 0, -1) == List(0, 1, -2, 3, 4)
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
