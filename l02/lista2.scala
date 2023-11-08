import scala.annotation.tailrec

def cutAndMend[A](a: Int, b: Int) = {
    (lst: List[A]) => {
        def aux(current: Int, lst: List[A]): List[A] = {
            (lst, a <= current && current <= b) match {
                case (Nil, _) => Nil
                case (h::t, false) => h::aux(current+1, t)
                case (h::t, true) => aux(current+1, t)
            }
        }
        aux(0, lst)
    }
}
def cutAndMendTests = {
    val test1 = cutAndMend(0, 0)(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == List(2, 3, 4, 5, 6, 7, 8, 9)
    val test2 = cutAndMend(1, 1)(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == List(1, 3, 4, 5, 6, 7, 8, 9)
    val test3 = cutAndMend(2, 2)(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == List(1, 2, 4, 5, 6, 7, 8, 9)
    val test4 = cutAndMend(-1, -2)(List(1, 2, 3)) == List(1, 2, 3)
    val test5 = cutAndMend(0, 2)(List(1, 2, 3)) == List()
    val test6 = cutAndMend(1, 5)(List()) == List()
    test1 && test2 && test3 && test4 && test5 && test6
}

def split2Rec[A](lst: List[A]) = {
    def aux(lst: List[A]): (List[A], List[A]) = {
        lst match {
            case Nil => (Nil, Nil)
            case _::Nil => (Nil, Nil)
            case x::y::rest => {
                val (a, b) = aux(rest)
                (x::a, y::b)
            }
        } 
    }
    aux(lst)
}

def split2RecTests = {
    val test1 = split2Rec(List(1,2,3,4)) == (List(1,3), List(2,4))
    val test2 = split2Rec(List(1,2,3,4,5)) == (List(1,3), List(2,4))
    val test3 = split2Rec(List(1,2,3,4,5,6)) == (List(1,3,5), List(2,4,6))
    val test4 = split2Rec(List()) == (List(), List())
    val test5 = split2Rec(List(1)) == (List(), List())
    val test6 = split2Rec(List(1,2)) == (List(1), List(2))
    test1 && test2 && test3 && test4 && test5 && test6
}

def split2Acc[A](lst: List[A]): (List[A], List[A]) = {
    def rev(lst: List[A]): List[A] = {
        @tailrec
        def aux(acc: List[A], lst: List[A]): List[A] = {
            lst match {
                case Nil => acc
                case h::t => aux(h::acc, t)
            }
        }
        aux(Nil, lst)
    }

    @tailrec
    def aux(lst: List[A], l1: List[A], l2: List[A]): (List[A], List[A]) = {
        lst match {
            case Nil => (l1, l2)
            case x :: Nil => (l1, l2)
            case x :: y :: rest => aux(rest, x::l1, y::l2)
        }
    }
    val (a, b) = aux(lst, Nil, Nil)
    (rev(a), rev(b))
}

def split2AccTests = {
    val test1 = split2Acc(List(1,2,3,4)) == (List(1,3), List(2,4))
    val test2 = split2Acc(List(1,2,3,4,5)) == (List(1,3), List(2,4))
    val test3 = split2Acc(List(1,2,3,4,5,6)) == (List(1,3,5), List(2,4,6))
    val test4 = split2Acc(List()) == (List(), List())
    val test5 = split2Acc(List(1)) == (List(), List())
    val test6 = split2Acc(List(1,2)) == (List(1), List(2))
    test1 && test2 && test3 && test4 && test5 && test6
}

def listRev[A](lst: List[A]) = {
    def aux(acc: List[A], lst: List[A]): List[A] = {
        lst match {
            case Nil => acc
            case h::t => aux(h::acc, t)
        }
    }
    aux(Nil, lst)
}

def split2RecOnHalf[A](lst: List[A]) = {
    def aux(lst: List[A], n: Int): ((List[A], List[A]), Int) = {
        lst match {
            case Nil => ((Nil, Nil), n)
            case h::t => {
                val ((lst1, lst2), lst_len) = aux(t, n+1)
                (n < lst_len/2, n < (lst_len / 2) * 2) match {
                    case (true, _) => ((h::lst1, lst2), lst_len)
                    case (_, true) => ((lst1, h::lst2), lst_len)
                    case _ => ((lst1, lst2), lst_len)
                }
            }
        }
    }
    aux(lst, 0)._1
}

def split2RecOnHalfTests = {
    val test1 = split2RecOnHalf(List(1,2,3,4)) == (List(1,2), List(3,4))
    val test2 = split2RecOnHalf(List(1,2,3,4,5)) == (List(1,2), List(3,4))
    val test3 = split2RecOnHalf(List(1,2,3,4,5,6)) == (List(1,2,3), List(4,5,6))
    val test4 = split2RecOnHalf(List()) == (List(), List())
    val test5 = split2RecOnHalf(List(1)) == (List(), List())
    val test6 = split2RecOnHalf(List(1,2)) == (List(1), List(2))
    test1 && test2 && test3 && test4 && test5 && test6
}

def listLen[A](lst: List[A]) = {
    def aux(acc: Int, lst: List[A]): Int = {
        lst match {
            case Nil => acc
            case h::t => aux(acc+1, t)
        }
    }
    aux(0, lst)
}

def split2AccOnHalf[A](lst: List[A]) = {
    val len = listLen(lst)
    @tailrec
    def aux(lst: List[A], acc1: List[A], acc2: List[A], n: Int): (List[A], List[A]) = {
        lst match {
            case Nil => (acc1, acc2)
            case h::t => {
                (n < len / 2, n < (len / 2) * 2) match {
                    case (true, _) => aux(t, h::acc1, acc2, n+1)
                    case (_, true) => aux(t, acc1, h::acc2, n+1)
                    case _ => (acc1, acc2)
                }
            }
        }
    }
    val (a, b) = aux(lst, Nil, Nil, 0)
    (listRev(a), listRev(b))
}

def split2AccOnHalfTests = {
    val test1 = split2AccOnHalf(List(1,2,3,4)) == (List(1,2), List(3,4))
    val test2 = split2AccOnHalf(List(1,2,3,4,5)) == (List(1,2), List(3,4))
    val test3 = split2AccOnHalf(List(1,2,3,4,5,6)) == (List(1,2,3), List(4,5,6))
    val test4 = split2AccOnHalf(List()) == (List(), List())
    val test5 = split2AccOnHalf(List(1)) == (List(), List())
    val test6 = split2AccOnHalf(List(1,2)) == (List(1), List(2))
    test1 && test2 && test3 && test4 && test5 && test6
}


def main(args: Array[String]) = {
    println(cutAndMendTests)
    println(split2RecTests)
    println(split2AccTests)
    println(split2RecOnHalfTests)
    println(split2AccOnHalfTests)
}