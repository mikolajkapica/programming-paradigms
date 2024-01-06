def skipTakeL[A](l: LazyList[A]): LazyList[A] =
    def aux(l: LazyList[A], n: Int, n_start: Int): LazyList[A] =
        l match {
        case LazyList() => LazyList()
        case h #:: t => if n == 0 then h #:: aux(t, n_start + 1, n_start + 1)
                  else aux(t, n - 1, n_start)
        }
    aux(l, 0, 0)

def main(args: Array[String]): Unit =
    val l = LazyList.from(-5)
    println(skipTakeL(l).take(10).toList)
    println(skipTakeL(l).take(0).toList)
    println(skipTakeL(LazyList()).take(10).toList)