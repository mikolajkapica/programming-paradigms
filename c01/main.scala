object Main {
  def main(args: Array[String]): Unit = {
    // val lista = List(List(1,2,3), List(5,6,3), List(2,253,6))
    // val out = zadanie1.flatten1(lista)
    // println(out)

    // val lista2 = List(1,2,3,3,4,3,3,5,3,2)
    // val out2 = zadanie2.countNum(lista2, 3)
    // println(out2)

    val lista5 = List('b', 'a', 'l', 'a', 'b')
    val out = zadanie5.palindrome(lista5)
    println(out)
  }
}

object zadanie1 {
  def flattenTwoLists(xs: List[Int], ys: List[Int]): List[Int] = {
    def aux(xs: List[Int], ys: List[Int]): List[Int] = {
      xs match {
        case Nil => ys
        case h :: t => aux(t, h :: ys)
      }
    }
    aux(xs.reverse, ys)
  }

  // 1 opcja:
  def flatten1(xss: List[List[Int]]): List[Int] = {
    def aux(lists: List[List[Int]], acc: List[Int]): List[Int] = {
      lists match {
        case Nil => acc
        case h :: Nil => aux(Nil, flattenTwoLists(h, acc))
        case h :: t => aux(t, flattenTwoLists(h, acc))
      }
    }
    aux(xss.reverse, List())
  }

  // 2 opcja:
  // def flatten1(xss: List[List[Int]]): List[Int] = {
  //   xss.fold(List())(flattenTwoLists)
  // }

}

object zadanie2 {
  def countNum(l: List[Int], num: Int): Int = {
    l.fold(0)((result, curr) => (if curr == num then result + 1 else result))

    // def aux(l: List[Int], acc: Int): Int = {
    //   l match {
    //     case h :: t if h == num => aux(t, acc + 1)
    //     case _ :: t => aux(t, acc)
    //     case Nil => acc
    //   }
    // }
    // aux(l, 0)
  }
}

object zadanie3 {
  def replicate(str: String, n: Int): List[String] = {
    def aux(n: Int, acc: List[String]): List[String] = {
      if n == 0 then acc
      else if n < 0 then throw new Exception("n must be positive")
      else aux(n-1, str :: acc)
    }
    aux(n, List())
  }
}

object zadanie4 {
}

object zadanie5 {
  def palindrome(l: List[Char]): Boolean = l == l.reverse
}

object zadanie6 {
  def listLength[A](xs: List[A]): Int = {
    xs match {
      case Nil => 0
      case _ :: t => 1 + listLength(t)
    }
  }
}

object zadanie7 {
  // dlugie
}

