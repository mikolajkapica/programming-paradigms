
def changeToNum[A](lst: List[A])(nums: List[A])(num: A): List[A] = {
    def contains(lst: List[A])(e: A): Boolean = {
        lst match {
            case Nil => false
            case h :: _ if h == e => true
            case _ :: t => contains(t)(e)
        }
    }
    lst match {
        case Nil => Nil
        case h :: t => {
            val lst = changeToNum(t)(nums)(num)
            if (contains(nums)(h)) {
                num :: lst
            } else {
                h :: lst
            }
        }
    }
}

def main(args: Array[String]) = {
    val res = changeToNum(List(1,2,3,4,5))(List(2,4))(0)
    println(res)
    val res2 = changeToNum(List(0))(List(0,2,4))(4)
    println(res2)
    val res3 = changeToNum(List(1,2,3))(List(0,2,4))(4)
    println(res3)
    val res4 = changeToNum(List())(List(0,2,4))(4)
    println(res4)
    val res5 = changeToNum(List(2,5,6))(List(0,1,4))(4)
    println(res5)
}