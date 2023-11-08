/**
  * Created by mkarol on 23.10.16.
  */
object App2 {
  // Zad 1


  // Zad 2
  def fibA(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => if (n > 0) { fibA(n-1) + fibA(n-2) } else { throw new Exception("Ujemny argument")}
    }
  }
  def fibB(n: Int): Int = {
    def fibIn(n: Int, p: Int, s: Int): Int = {
      n match {
        case 0 => p
        case 1 => s
        case _ => fibIn(n - 1, s, p+s)
      }
    }
    if (n >= 0) fibIn(n, 0, 1)
    else throw new Exception("Ujemny argument")
  }

  // Zad 3
  def root3(a: Double, e: Double): Double = {
    def fabs(a: Double): Double = {
      if (a >= 0) a
      else -a
    }
    def cbrtIn(a: Double, i: Int, s: Double, e: Double): Double = {
      if (fabs(s*s*s-a) < e*fabs(a)) {
        s
      } else {
        i match {
          case 0 if a > 1 => cbrtIn(a, i+1, a/3, e)
          case 0 => cbrtIn(a, i+1, a, e)
          case _ => cbrtIn(a, i+1, s + (a/(s*s)-s)/3, e)
        }
      }
    }
    if (e < 0) throw new Exception("Dokładność musi być nieujemna")
    cbrtIn(a, 0, 0, e)
  }

  // Zad 4
  def matchingA[T](a:List[T]): Unit = {
    val List(_, _, x, _, _) = a
    println(x)
  }
  def matchingB[T](a:List[T]): Unit = {
    val List(_, (x, _)) = a
    println(x)
  }

  // Zad 5
  def initSegment[T](l: (List[T], List[T])): Boolean ={
    l match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (x, y) => if (x.head == y.head) initSegment(x.tail, y.tail) else false
    }
  }

  // Zad 6
  def replaceNth[T](list :List[T], pos: Int, elem: T): List[T] = {
    def replace[T](list :List[T], pos: Int, elem: T, index: Int, ret:List[T]): List[T] = {
      if (pos == index) {
        ret.reverse++(elem::list.tail)
      } else {
        replace(list.tail, pos, elem, index + 1, list.head :: ret)
      }
    }
    if (pos < 0 || pos > list.length-1) { throw new Exception("Błędna długość")}
    replace(list, pos, elem, 0, Nil)
  }

  def main(args: Array[String]): Unit = {
    println(fibA(17))
    println(fibB(17))

    println(root3(1001, 1))

    matchingA(List(-2, -1, 0, 1, 2))
    matchingB(List((1, 2), (0, 1)))

    println(initSegment(Nil, Nil))
    println(initSegment(List(1, 2), Nil))
    println(initSegment(Nil, List(1, 2)))
    println(initSegment(List(1, 3), List(1, 2, 5)))
    println(initSegment(List(1, 3), List(1, 3, 5)))

    println(replaceNth(List(0, 1, 2, 3, 4, 5), 3, 10))
    println(replaceNth(Nil, 0, 10))
  }
}

