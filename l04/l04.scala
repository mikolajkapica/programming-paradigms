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

    def printTree3(t: Tree3[Int]): Unit = 
        def aux(t: Tree3[Int], lvl: Integer): Unit = 
            t match {
                case Empty => println(" " * lvl + "Empty")
                case Node(e, l, c, r) =>
                    println(" " * lvl + e)
                    aux(l, lvl+1)
                    aux(c, lvl+1)
                    aux(r, lvl+1)
            }
        aux(t, 0)

    printTree3.compose(mapTree3((x: Int) => x * 2))(t3)


// 2 --------------------------
type Data = String
type Name = String
type Letter = Char

sealed trait Item
case class File(name: Name, data: Data) extends Item
case class Folder(name: Name, items: List[Item]) extends Item

case class Disk(letter: Letter, items: List[Item])

val d =
    Disk ('C', List(
        File("autoexec.bat", "file"),
        File("config.sys", "file"),
        Folder("Windows", List(
            File("win.exe", "file"),
            File("win.ini", "file"),
            Folder("System", List(
                File("kernel.sys", "file"),
                File("command.com", "file")
            ))
        ))
    ))

def main(args: Array[String]): Unit = 
    test_tree