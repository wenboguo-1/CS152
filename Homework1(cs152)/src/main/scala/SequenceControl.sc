import scala.util.Random
import scala.util.control.Breaks.{break, breakable}
import java.lang.System.currentTimeMillis
import scala.util._
//****************************//
def kingdom(n: Int) = {
  if (n <= 10)
    3
  else if (n > 10) {
    if (n % 2 == 0)
      1
    else if (100%n == 0)
      2
    else
      4
  }
}
println("Number Kingdoms")
println("12 belongs to kingdom-> " + kingdom(25))
println("99 belongs to kingdom-> " + kingdom(99))
println("10 belongs to kingdom-> " + kingdom(10))
println("12 belongs to kingdom-> " + kingdom(12))
/*
 99 belongs to kingdom-> 4
 10 belongs to kingdom-> 3
 12 belongs to kingdom-> 1
 */

//****************************//
def order(n: Int): Int = {
  var family = 0;
  var genus = 0;
  var classs = 0
  if (n < 0)
    throw new Exception
  else {
    if (n % 3 == 0)
      family = 1
    else
      family = 2
    if (n == 50)
      classs = 3
    else
      classs = 4
    if (n % 7 == 0)
      genus = 5
    else
      genus = 6
  }
  family * classs + genus
}
println()
println("Number Orders")
try {
    println("The order 3 is " + order(3))
    println("The order 10 is " +order(10))
    println("The order 15 is " + order(15))
    println("The order -1 is " + order(-1))
} catch {
  case e: Exception => println("Bad Data")
}
/*     Output
       The order 3 is 10
       The order 10 is 14
       The order 15 is 10
       Bad Data
   */


//****************************//
def species(n: Int): Int = {
  /*
      def species(n: Int) =
       if (0 < n) if (n % 2 == 0) 1 else 2
       this statement is wrong because the second if statement
       is in the range of the first if statement so it only executes
       when n is less 0;
   */
  if (n < 0 || n % 2 != 0)
    2
  else
    1
}

println()
println("Number Species")
println("Species of a number for 3 is " + species(3))
println("Species of a number for 6 is " + species(4))
println("Species of a number for -3 is " + species(-3))
/*     Output
       Species of a number for 3 is 2
       Species of a number for 6 is 1
       Species of a number for -3 is 2
  */

//****************************//
def tax(n: Double) = {
  if (n < 0)
    throw new Exception("Bad data")
  n match {
    case n if n < 20000 => n * 0.0;
    case n if n < 30000 => n * 0.15
    case n if n < 40000 => n * 0.11
    case n if n < 60000 => n * 0.23
    case n if n < 100000 => n * 0.32
    case _ => n * 0.5;
  }
}

println()
println("Elbonian Tax Calculator")
println(tax(12300))
println(tax(29000))
println(tax(125000))
println(tax(1000000))
try {
  println(tax(-10000))
} catch {
  case e: Exception => println(e)
}
/*
    0.0
    4350.0
    62500.0
    500000.0
    java.lang.Exception: Bad data
  */

//****************************//
def drawRectangle(width: Int, length: Int) {
  for (i <- 1 to width) {
    for (j <- 1 to length) {
      print("*")

    }
    println();
  }
}
println()
println("DrawRectangle")
drawRectangle(12, 13)
/*
  *************
  *************
  *************
  *************
  *************
  *************
  *************
  *************
  *************
  *************
  *************
  *************
 */


//****************************//
def printSum(n: Int, m: Int) {
  for (i <- 0 to n - 1) {
    for (j <- 0 to m - 1) {
      print(i + " + " + j + " = " + (i + j));
      println()
    }
  }

}

println()
println("PrintSums")
printSum(3, 4)
/*
    0 + 0 = 0
    0 + 1 = 1
    0 + 2 = 2
    0 + 3 = 3
    1 + 0 = 1
    1 + 1 = 2
    1 + 2 = 3
    1 + 3 = 4
    2 + 0 = 2
    2 + 1 = 3
    2 + 2 = 4
    2 + 3 = 5
 */

//*****************************//
object BlackJack1{
  val gen = new Random(currentTimeMillis())
  def getResult(): Int = {
    val cards = new Array[Int](52)
    for (i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1
    var total = 0
    breakable {
      for (i <- 0 until 52) {
        breakable {
          if (cards(i) < 0)
            break
        }
        if (total >= 21) {
          break
        }
        total = total + cards(i);
      }
    }
    total
  }

}
println()
println("The black jack(versioin 1) value is -> " + BlackJack1.getResult())
println("The black jack(versioin 1) value is -> " + BlackJack1.getResult())
println("The black jack(versioin 1) value is -> " + BlackJack1.getResult())
 /*
    Output
    The black jack(versioin 1) value is -> 22
    The black jack(versioin 1) value is -> 26
    The black jack(versioin 1) value is -> 21
*/

//****************************//
object BlackJack2{

  def getResult(): Int = {

      val gen = new Random(currentTimeMillis())
      val cards = new Array[Int](52)
      for (i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1
      var total = 0
      try {
        for (i <- 0 until 52) {
          try {
            if (cards(i) < 0)
              throw new Exception
          } catch {
            case _: Throwable =>
          }
          if (total >= 21)
            throw new Exception
          total = total + cards(i);
        }
      } catch {
        case _: Throwable =>
      }
      total
  }
}
println()
println("The black jack(version 2) value is -> " + BlackJack2.getResult())
println("The black jack(version 2) value is -> " + BlackJack2.getResult())
println("The black jack(version 2) value is -> " + BlackJack2.getResult())
/*
   Output
   The black jack(version 2) value is -> 26
   The black jack(version 2) value is -> 24
   The black jack(version 2) value is -> 28
*/

//*****************************//
object BlackJack3 {

  def getResult(): Int = {
    val gen = new Random(currentTimeMillis())
    val cards = new Array[Int](52)
    for (i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1
    var total = 0
    for (i <- 0 until 53 if total <= 21) {
      if (cards(i) >= 0)
        total = total + cards(i)
    }
    total
  }
}
println()
println("The black jack(version 3) value is -> " + BlackJack3.getResult())
println("The black jack(version 3) value is -> " + BlackJack3.getResult())
println("The black jack(version 3) value is -> " + BlackJack3.getResult())
/*
   Output
   The black jack(version 3) value is -> 23
   The black jack(version 3) value is -> 23
   The black jack(version 3) value is -> 25
*/

//****************************//
def realm1(n:Int): Int = {
  n
}

def realm2(n:Int): Int = {
  n
}

def realm3(n:Int): Int = {
  n
}
def findRealm(n: Int): Int = {
  if (n > 0 && n % 2 != 0)
    realm1(1)
  else if (n >= 0 && n % 2 == 0 && n % 3 != 0)
    realm2(2)
  else if (n >= 0 && (n % 6 == 0 && n % 7 == 0))
    realm3(3)
  else
    throw new Exception("It belongs to realm 0")
}
println()
println("realmN")
println("Number n is 13, and it belongs to realm " + findRealm(13))
println("Number n is 10, and it belongs to realm " + findRealm(10))
println("Number n is 42, and it belongs to realm " + findRealm(42))
try {
  println(findRealm(54))
} catch {
  case e: Exception => println(e)
}
/*
 realmN
 Number n is 13, and it belongs to realm 1
 Number n is 10, and it belongs to realm 2
 Number n is 42, and it belongs to realm 3
 java.lang.Exception: It belongs to realm 0
 */

//****************************//
def sqrt(value: Option[Double]): Option[Double] = {
  value match {
    case None => None
    case Some(x) => Some(math.sqrt(x))
  }
}

def log(x: Double): Option[Double] = {
  x match {
    case x if x < 1 => None
    case _ => Some(math.log(x))
  }
}

def sqrtLog(n: Double) = {
  sqrt(log(n))
}

println()
println("Monadic Bind")
println("Input -> 0.1: " + sqrtLog(0.1))
println("Input -> 25: " + sqrtLog(25))
/*
    Monadic Bind
    Input -> 0.1: None
    Input -> 25: Some(1.7941225779941015)
*/


