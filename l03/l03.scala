val prime = (n: Int) => (for (x <- List.range(2,n) if n % x == 0) yield x) == Nil

