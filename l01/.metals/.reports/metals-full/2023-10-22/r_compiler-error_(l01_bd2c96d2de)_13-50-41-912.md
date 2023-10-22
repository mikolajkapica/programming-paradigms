file://<WORKSPACE>/l01.scala
### java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      = new IllegalArgumentException(null) # -1,
parent span = <1095..1494>,
child       = null # -1,
child span  = [1124..1495..1495]

occurred in the presentation compiler.

action parameters:
offset: 1133
uri: file://<WORKSPACE>/l01.scala
text:
```scala
def reverse4[A](x1: A, x2: A, x3: A, x4: A): (A, A, A, A) = (x4, x3, x2, x1)

def reverse4_tests =
    val test1 = reverse4(1, 2, 3, 4) == (4, 3, 2, 1)
    val test2 = reverse4(1, 2, 3, 4) != (1, 2, 3, 4)
    val test3 = reverse4(0, 0, 0, 0) == (0, 0, 0, 0)
    val test4 = reverse4(-1, -2, -3, -4) == (-4, -3, -2, -1)
    test1 && test2 && test3 && test4

def sumProd(s: Int, e: Int): (Int, Int) = {
  if (s >= e) throw new IllegalArgumentException("s > e")
  def aux(current: Int, sum: Int, prod: Int): (Int, Int) = {
    if (current > e) (sum, prod)
    else aux(current + 1, sum + current, prod * current)
  }
  aux(s, 0, 1)
}

def sumProd_tests =
    val test1 = sumProd(1, 5) == (15, 120)
    val test2 = sumProd(1, 2) == (3, 2)
    val test3 = sumProd(1, 10) == (55, 3628800)
    val test4 = sumProd(-4, 10) == (-4+(-3)+(-2)+(-1)+0+1+2+3+4+5+6+7+8+9+10, 0)
    val test5 = sumProd(-4, -1) == (-4+(-3)+(-2)+(-1), (-4)*(-3)*(-2)*(-1))
    val test6 = sumProd(1, 5) == (15, 120)
    test1 && test2 && test3 && test4 && test5 && test6

def isPerfect(n: Int): Boolean = {
    if (n < 1) throw new IllegalArgumentException("n must b@@
    val divisors = {
        def aux(current: Int, divisors: List[Int]): List[Int] = {
            if (current == 0) divisors
            else if (n % current == 0) aux(current - 1, current :: divisors)
            else aux(current - 1, divisors)
        }
        aux(n-1, List())
    }
    val divisors_sum = divisors.foldLeft(0)(_ + _)
    divisors_sum == n
}

def isPerfect_tests =
    val test1 = isPerfect(6) == true
    val test2 = isPerfect(28) == true
    val test3 = isPerfect(496) == true
    val test4 = isPerfect(7) == false
    val test5 = isPerfect(-6) == false
    val test6 = isPerfect(0) == false
    test1 && test2 && test3 && test4 && test5 && test6

def insert[A](lst: List[A], x: A, pos: Int): List[A] = {
    def aux(lst: List[A], current: Int): List[A] =
        lst match {
            case Nil => List(x)
            case h :: t => if (current == pos) x :: lst else h :: aux(t, current + 1)
        }
    if pos < 0 then x :: lst 
    else aux(lst, 0)
}

def insert_tests = {
    val test1 = insert(List(1, 2, 3, 4), 0, 2) == List(1, 2, 0, 3, 4)
    val test2 = insert(List(1, 2, 3, 4), 0, 0) == List(0, 1, 2, 3, 4)
    val test3 = insert(List(), 0, 4) == List(0)
    val test4 = insert(List(1), 0, 5) == List(1, 0)
    val test5 = insert(List(1, -2, 3, 4), 0, -1) == List(0, 1, -2, 3, 4)
    test1 && test2 && test3 && test4 && test5
}


object Main {
    def main(args: Array[String]): Unit = {
        println("reverse4_tests: " + reverse4_tests)
        println("sumProd_tests: " + sumProd_tests)
        println("isPerfect_tests: " + isPerfect_tests)
        println("insert_tests: " + insert_tests)
    }
}

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:175)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$1(ParserPhase.scala:38)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$adapted$1(ParserPhase.scala:39)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:437)
	dotty.tools.dotc.parsing.Parser.parse(ParserPhase.scala:39)
	dotty.tools.dotc.parsing.Parser.runOn$$anonfun$1(ParserPhase.scala:48)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.parsing.Parser.runOn(ParserPhase.scala:48)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:247)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1321)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:263)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:271)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:280)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:67)
	dotty.tools.dotc.Run.compileUnits(Run.scala:280)
	dotty.tools.dotc.Run.compileSources(Run.scala:195)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:42)
	scala.meta.internal.pc.PcDocumentHighlightProvider.<init>(PcDocumentHighlightProvider.scala:16)
	scala.meta.internal.pc.ScalaPresentationCompiler.documentHighlight$$anonfun$1(ScalaPresentationCompiler.scala:155)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      = new IllegalArgumentException(null) # -1,
parent span = <1095..1494>,
child       = null # -1,
child span  = [1124..1495..1495]