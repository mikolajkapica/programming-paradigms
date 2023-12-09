def skipTakeL[A](l: LazyList[A]): LazyList[A] =
    def aux(l: LazyList[A], n: Int, n_start: Int): LazyList[A] =
        l match {
        case LazyList() => LazyList()
        case _ => if n == 0 then l.head #:: aux(l.tail, n_start + 1, n_start + 1)
                  else aux(l.tail, n - 1, n_start)
        }
    aux(l, 0, 0)

def main(args: Array[String]): Unit =
    val l = LazyList.from(1)
    println(skipTakeL(l).take(10).toList)
    println(skipTakeL(LazyList()).take(10).toList)