val prime = (n: Int) => Nil == (for (x <- List.range(2, Math.sqrt(n).toInt + 1) if n % x == 0) yield x)

val primes = (n: Int) => (for (x <- List.range(2, n) if prime(x)) yield x)

// erastostenes sieve by for comprehension
val sieved = () =>
    val sieve = Array.fill[Boolean](100)(true)
    for {
        p <- 2 until sieve.length 
        if sieve(p) && math.pow(p, 2) <= sieve.length
        i <- p * 2 until sieve.length by p
    } sieve(i) = false

val tests = 
    val l = List(
        prime(2) == true,
        prime(3) == true,
        prime(4) == false,
        prime(5) == true,
        prime(6) == false,
        prime(7) == true,
        prime(8) == false,
        prime(9) == false,
        prime(10) == false,
        prime(11) == true,
        prime(12) == false,
        prime(13) == true,
        prime(14) == false,
        prime(15) == false,
        prime(16) == false,
        prime(17) == true,
        prime(18) == false,
        prime(19) == true,
        prime(20) == false,
        prime(21) == false,
        prime(22) == false,
        prime(23) == true,
        prime(24) == false,
        prime(25) == false,
        prime(26) == false,
        prime(27) == false,
        prime(28) == false,
        prime(29) == true,
        prime(30) == false,
        primes(2) == List(),
        primes(3) == List(2),
        primes(4) == List(2, 3),
        primes(5) == List(2, 3),
        primes(6) == List(2, 3, 5),
        primes(7) == List(2, 3, 5),
        primes(8) == List(2, 3, 5, 7),
        primes(9) == List(2, 3, 5, 7),
        primes(10) == List(2, 3, 5, 7),
    )
    l.forall(_ == true)


