/*
 * Lista 9 - Programowanie obiektowe II - Scala
 */

trait Figure {
    def area: Double
}

class Rectangle(private var _a: Double, private var _b: Double = 0) extends Figure {
    if _a <= 0 || _b < 0 then throw new IllegalArgumentException("a and b must be positive")
    if (b == 0) b = a 

    def a = _a

    def a_=(x: Double) = {
        if x < 0 then throw new IllegalArgumentException
        else _a = x
    }

    def b = _b

    def b_=(x: Double) = {
        if x < 0 then throw new IllegalArgumentException
        else _b = x
    }

    override def area = a * b
}

class Splitter(threshold: Double) {
  if (threshold <= 0) throw new IllegalArgumentException("Threshold must be positive")

  private var smallFigures: List[Figure] = Nil
  private var bigFigures: List[Figure] = Nil

  def apply(f: Figure): Unit = {
    if (f.area < threshold) smallFigures = f :: smallFigures
    else bigFigures = f :: bigFigures
  }

  def printSmallFigures(): Unit = {
    println("Small Figures:")
    smallFigures.foreach(f => println(s"Pole: ${f.area}"))
  }

  def printBigFigures(): Unit = {
    println("Big Figures:")
    bigFigures.foreach(f => println(s"Pole: ${f.area}"))
  }
}

@main def m =
  var s = Splitter(1500) 
  var rectangles = List(
    Rectangle(10, 20),
    Rectangle(30),
    Rectangle(30, 40),
    Rectangle(40, 50),
    Rectangle(60),
    Rectangle(60, 70),
    Rectangle(70, 80),
  )
  // rectangles(1)._a = 5 // error
  rectangles(2).a = 5 // actually a_= method is called
  rectangles.foreach(s.apply)

  s.printSmallFigures()
  s.printBigFigures()


    
