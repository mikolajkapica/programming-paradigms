/* 
Lista 6
Efekty obliczeniowe. Programowanie imperatywne
*/

// a ////////////////////////////////////////
def fib(n: Int): Int =
    n match
        case n if n < 1 => 0
        case n if n < 3 => 1
        case n => fib(n-1) + fib(n-2)
    

def skip(pair: (Int, Int)): Int =
    pair match
        case (m, n) if m < 1 || n < 1 => throw new IllegalArgumentException("skipponacci (m, n): m < 1 || n < 1")
        case (1, n) => fib(n)
        case (m, n) => skip (m-1, n) + skip (m-1, n+1)

def make_skipponacci_list(m: Int, num_of_elem: Int): List[Int] =
    def aux(acc: List[Int], last_two: (Int, Int), n: Int): List[Int] =
        n match
            case 0 => acc
            case n => last_two match
                case (a, b) => aux (a::acc, (b, a+b), n-1)
    val fst_e = skip (m, 1)
    val snd_e = skip (m, 2)
    aux (Nil, (fst_e, snd_e), num_of_elem).reverse

// b ////////////////////////////////////////
def fib_imp(n: Int): Int =
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

def skip_imp(m: Int, n: Int): Int = 
    fib_imp (2 * (m - 1) + n)

/* 
    PROOF
        Let's guess the formula of skipponacci (m, n) = fib (2 * (m - 1) + n)
    Base case: 
        skipponacci(1,n) = fib (2 * (m - 1) + n)
    Induction step:
        IH: skipponacci (m, n) = fib (2 * (m - 1) + n)
        Then: skipponacci (m+1, n) = fib (2 * (m - 1) + n + 2)
        skipponacci (m+1, n) = skipponacci (m, n) + skipponacci (m, n+1)
                             = fib (2 * (m - 1) + n) + fib (2 * (m - 1) + n + 1)
                             [knowing that fib n + fib (n+1) = fib (n+2)]
                             = fib (2 * (m - 1) + n + 2)
    QED
*/

def make_skipponacci_array(m: Int, num_of_elem: Int): Array[Int] =
    val arr = Array.ofDim[Int](num_of_elem)
    var a = skip_imp (m, 1)
    var b = skip_imp (m, 2)
    var c = 0
    for (i <- 0 to num_of_elem - 1) {
        arr(i) = a
        c = a + b
        a = b
        b = c
    }
    arr

def tests: Boolean =
    val test1 = make_skipponacci_list (1, 10) ==           List(1,1,2,3,5,8,13,21,34,55)
    val test2 = make_skipponacci_array(1, 10) sameElements Array(1,1,2,3,5,8,13,21,34,55)
    val test3 = make_skipponacci_list (2, 10) ==           List(2,3,5,8,13,21,34,55,89,144)
    val test4 = make_skipponacci_array(2, 10) sameElements Array(2,3,5,8,13,21,34,55,89,144)
    val test5 = make_skipponacci_list (3, 10) ==           List(5,8,13,21,34,55,89,144,233,377)
    val test6 = make_skipponacci_array(3, 10) sameElements Array(5,8,13,21,34,55,89,144,233,377)
    val test7 = make_skipponacci_list (4, 10) ==           List(13,21,34,55,89,144,233,377,610,987)
    val test8 = make_skipponacci_array(4, 10) sameElements Array(13,21,34,55,89,144,233,377,610,987)
    test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8