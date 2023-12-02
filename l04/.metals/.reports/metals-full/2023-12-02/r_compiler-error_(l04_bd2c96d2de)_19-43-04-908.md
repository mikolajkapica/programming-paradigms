file://<WORKSPACE>/l04.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

action parameters:
offset: 1304
uri: file://<WORKSPACE>/l04.scala
text:
```scala
// 1 --------------------------
sealed trait Tree3[+A] 
case class Node[A](element: A, left: Tree3[A], center:  Tree3[A], right: Tree3[A]) extends Tree3[A]
case object Empty extends Tree3[Nothing]

def mapTree3[A, B](f: A => B): Tree3[A] => Tree3[B] =
    (t: Tree3[A]) => t match {
        case Empty => Empty
        case Node(e, l, c, r) => Node(f(e), mapTree3(f)(l), mapTree3(f)(c), mapTree3(f)(r))
    }

// test map tree with printing
val test_tree = 
    val t3 = 
        Node (1, 
            Node (2, 
            Node (3, Node (4, Empty, Empty, Empty), Empty, Empty), 
            Node (5, Empty, Empty, Empty), 
            Node (6, Empty, Empty, Empty)
            ), 
            Node (7, 
            Node (8, Empty, Empty, Empty), 
            Node (9, Empty, Empty, Empty), 
            Node (10, Empty, Empty, Empty)
            ), 
            Node (11, 
            Node (12, Empty, Empty, Empty), 
            Node (13, Empty, Empty, Empty), 
            Node (14, Empty, Empty, Empty)
            )
        ) 

    def printTree3(t: Tree3[Int], lvl: int): Unit = 
        t match {
        // print lvl spaces and then print "Empty"
        case Empty => println(" " * lvl + "Empty")
        case Node(e, l, c, r) => {
            println(" " * lvl + e)
            printTree3(l, lv@@)
            printTree3(c)
            printTree3(r)
        }
    }

    printTree3(mapTree3((x: Int) => x * 2)(t3))


// 2 --------------------------
type Data = String
type Name = String
type Letter = Char

sealed trait Item
case class File(name: Name, data: Data) extends Item
case class Folder(name: Name, items: List[Item]) extends Item

case class Disk(letter: Letter, items: List[Item])

def main(args: Array[String]): Unit = 
    test_tree
```



#### Error stacktrace:

```
java.base/java.util.TimSort.mergeLo(TimSort.java:781)
	java.base/java.util.TimSort.mergeAt(TimSort.java:518)
	java.base/java.util.TimSort.mergeForceCollapse(TimSort.java:461)
	java.base/java.util.TimSort.sort(TimSort.java:254)
	java.base/java.util.Arrays.sort(Arrays.java:1441)
	scala.collection.SeqOps.sorted(Seq.scala:727)
	scala.collection.SeqOps.sorted$(Seq.scala:719)
	scala.collection.immutable.List.scala$collection$immutable$StrictOptimizedSeqOps$$super$sorted(List.scala:79)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted(StrictOptimizedSeqOps.scala:78)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted$(StrictOptimizedSeqOps.scala:78)
	scala.collection.immutable.List.sorted(List.scala:79)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:210)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:86)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:123)
```
#### Short summary: 

java.lang.IllegalArgumentException: Comparison method violates its general contract!