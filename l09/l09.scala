/*
 * Lista 9 - Programowanie obiektowe II - Scala
 */

trait Figure {
    def area: Double
}

class Rectangle(private var _a: Double, private var _b: Double) extends Figure {
    if _a <= 0 || _b < 0 then throw new IllegalArgumentException("a and b must be positive")

    def this(side: Double) = this(side, side)

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

// (-inf, treshold] (treshold, +inf)
class Splitter(threshold: Double) {
  if (threshold <= 0) throw new IllegalArgumentException("Threshold must be positive")

  private var smallFigures: List[Figure] = Nil
  private var bigFigures: List[Figure] = Nil

  def apply(f: Figure): Unit = {
    if (isSmaller(f)) smallFigures =  f :: smallFigures 
    else bigFigures = f :: bigFigures 
  }

  protected def isSmaller(f: Figure): Boolean = f.area <= threshold

  protected def isBigger(f: Figure): Boolean = f.area > threshold

  protected def putIntoSmaller(f: Figure): Unit = smallFigures = f :: smallFigures

  protected def putIntoBigger(f: Figure): Unit = bigFigures = f :: bigFigures

  def printSmallFigures(): Unit = {
    println("Small Figures:")
    smallFigures.foreach(f => println(s"Pole: ${f.area}"))
  }

  def printBigFigures(): Unit = {
    println("Big Figures:")
    bigFigures.foreach(f => println(s"Pole: ${f.area}"))
  }
}

// (-inf, tresholdA] (tresholdA, tresholdB] (tresholdB, +inf)
class FineSplitter(tresholdA: Double, tresholdB: Double) extends Splitter(tresholdA) {
  if (!(tresholdA < tresholdB)) throw new IllegalArgumentException("TresholdB has to be bigger B then TresholdA")

  private var mediumFigures: List[Figure] = Nil

  override def apply(f: Figure): Unit = {
    if tresholdA < f.area && f.area <= tresholdB then putIntoMedium(f) else super.apply(f)
  }

  protected def isMedium(f: Figure): Boolean = f.area <= tresholdB

  protected def putIntoMedium(f: Figure): Unit = mediumFigures = f :: mediumFigures

  def printMediumFigures(): Unit = 
    println("Medium Figures")
    mediumFigures.foreach(f => println(s"Pole: ${f.area}"))
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
  
  println("FineSplitter")
  var s2 = FineSplitter(1000, 3000) 
  var rectangles2 = List(
    Rectangle(10, 20),
    Rectangle(30),
    Rectangle(30, 40),
    Rectangle(40, 50),
    Rectangle(60),
    Rectangle(60, 70),
    Rectangle(70, 80),
  )
  // rectangles2(1)._a = 5 // error
  rectangles2(2).a = 5 // actually a_= method is called
  rectangles2.foreach(s2.apply)

  s2.printSmallFigures()
  s2.printMediumFigures()
  s2.printBigFigures()


    
