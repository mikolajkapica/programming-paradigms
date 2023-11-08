
def argmax(lst: List[Int]): List[Int] = {
    def aux(lst: List[Int], currentIdx: Int, maxNum: Int, indecies: List[Int]): List[Int] = {
        if lst == Nil then indecies.reverse
        else if (lst.head > maxNum) then aux(lst.tail, currentIdx + 1, lst.head, currentIdx :: Nil)
        else if (lst.head == maxNum) then aux(lst.tail, currentIdx + 1, maxNum, currentIdx :: indecies)
        else aux(lst.tail, currentIdx + 1, maxNum, indecies)
    }
    if lst == Nil then Nil
    else aux(lst, 0, lst.head, Nil)
}

// def argmax(lst: List[Int]): List[Int] = {
//     def aux(lst: List[Int], currentIdx: Int, maxNum: Int): List[Int] = {
//         if lst == Nil then Nil
//         else if lst.head > maxNum then (aux(lst.tail, currentIdx + 1, lst.head))
//         else if lst.head == maxNum then currentIdx :: (aux(lst.tail, currentIdx + 1, maxNum))
//         else aux(lst.tail, currentIdx + 1, maxNum)
//     }
//     if lst == Nil then Nil
//     else aux(lst, 0, lst.head)
// }

def tests = {
    val test1 = (argmax(List(7,2,1,3,7,0)) == List(0,4))
    val test2 = (argmax(List(-1, -5, -4, -2, -3)) == List(0))
    val test3 = (argmax(List()) == List())
    val test4 = (argmax(List(-10)) == List(0))
    val test5 = (argmax(List(10)) == List(0))
    val test6 = (argmax(List(7,7,7)) == List(0,1,2))
    test1 && test2 && test3 && test4 && test5 && test6
}


object Main {
    def main(args: Array[String]): Unit = {
        println("tests: " + tests)
    }
}
