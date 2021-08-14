import scala.util.Random
import scala.util.control.Breaks.breakable
import scala.util.control.Breaks.break

//***************************************************************
object poly  {
  var firstX = 2
  var secondX = 1

  def valueOfNoRoot(p: (Double, Double, Double)): Double = {
    val (a, b, c) = p
    (b * b - 4 * a * c)
  }

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    var (a, b, c) = p
    val temp = valueOfNoRoot(p)
    temp match {
      case temp if temp < 0 => None
      case _ => Some((-b + math.sqrt(temp)) / (2 * a), (-b - math.sqrt(temp)) / (2 * a))
    }
  }

  def deriv(p: (Double, Double, Double)): (Double, Double, Double) = {
    val (a, b, c) = p;
    if (firstX == 0)
      (0, 0, 0)
    else if (secondX == 0)
      (a * firstX, 0, 0)
    else {
      val temp1 = firstX
      val temp2 = secondX
      firstX -= 1
      secondX -= 1
      (a * temp1, b * temp2, 0)
    }
  }

  def resetX(): Unit = {
    firstX = 2
    secondX = 1
  }

  def eval(h: Double, p: (Double, Double, Double)): Option[Double] = {
    val (a, b, c) = p;
    val temp = valueOfNoRoot(p)
    temp match {
      case temp if temp < 0 => None
      case _ => {
        val val1 = (-b + math.sqrt(temp)) / (2 * a) * -1
        val val2 = (-b - math.sqrt(temp)) / (2 * a) * -1
        if (val1 + h == 0 || val2 + h == 0)
          Some(0)
        else
          Some((val1 + h) * (val2 + h))
      }
    }
  }
}
//********************************************************************//
val p = (3.0, 9.0, -30.0)
  println("eval(6, p) = " + poly.eval(6, p))
  println("eval(2, p) = " + poly.eval(2, p))
  println("eval(-5, p) = " + poly.eval(-5, p))
  println("roots(p) = " + poly.roots(p))
  println("deriv(p) = " + poly.deriv(p))
  poly.resetX()
  println("deriv2(p) = " + poly.deriv(poly.deriv(p)))
  poly.resetX()
val p2 = (3.0, 9.0, 30.0)
println("eval(6, p) = " + poly.eval(6, p2))
println("eval(2, p) = " + poly.eval(2, p2))
println("eval(-5, p) = " + poly.eval(-5, p2))
println("roots(p) = " + poly.roots(p2))
println("deriv(p) = " + poly.deriv(p2))
poly.resetX()
println("deriv2(p) = " + poly.deriv(poly.deriv(p2)))
//*********************************************************************//
        //OUTPUT FOR POLY
/*
      val p: (Double, Double, Double) = (3.0,9.0,-30.0)
      eval(6, p) = Some(44.0)
      eval(2, p) = Some(0.0)
      eval(-5, p) = Some(0.0)
      roots(p) = Some((2.0,-5.0))
      deriv(p) = (6.0,9.0,0.0)
      deriv2(p) = (6.0,0.0,0.0)

      val p2: (Double, Double, Double) = (3.0,9.0,30.0)
      eval(6, p) = None
      eval(2, p) = None
      eval(-5, p) = None
      roots(p) = None
      deriv(p) = (6.0,9.0,0.0)
      deriv2(p) = (6.0,0.0,0.0)
 */

//***************************************************************//
object vector {
  def sum(v1: (Double, Double, Double), v2: (Double, Double, Double)): (Double, Double, Double) = {
    val (a11, a21, a31) = v1
    val (a12, a22, a32) = v2
    (a11 + a12, a21 + a22, a31 + a32)
  }

  def mul(a: Double, v: (Double, Double, Double)): (Double, Double, Double) = {
    val (a11, a21, a31) = v
    (a11 * a, a21 * a, a31 * a)
  }

  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double = {
    val (a11, a21, a31) = v1;
    val (a12, a22, a32) = v2

    (a11 * a12 + a21 * a22 + a31 * a32)
  }

  def length(v: (Double, Double, Double)): Double = {
    val (a1, a2, a3) = v
    math.sqrt(a1 * a1 + a2 * a2 + a3 * a3)

  }

  def theta(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double = {

    val length1 = length(v1)
    val length2 = length(v2)
    val tempDot = dot(v1, v2)
    math.acos(tempDot / (length1 * length2))
  }
}
 //********************************************************//
  val v1 = (2.0, 2.0, 2.0)
  val v2 = (1.0, 0.0, 0.0)
  val v3 = (0.0, 1.0, 0.0)

  println("sum(v3, v2) = " + vector.sum(v3, v2))
  println("mul(3, v1) = " + vector.mul(3, v1))

  println("dot(v1, v2) = " + vector.dot(v1, v2))
  println("dot(v2, v3) = " + vector.dot(v2, v3))
  println("dot(v1, v1) = " + vector.dot(v1, v1))

  println("length(v1) = " + vector.length(v1))
  println("length(v2) = " + vector.length(v2))

  println("theta(v1, v2) = " + vector.theta(v1, v2))
  println("theta(v3, v2) = " + vector.theta(v3, v2))
  println("pi/2 = " + Math.PI / 2)
  //****************************************************//
         //OUTPUT FOR VECTOR
  /*
          val v1: (Double, Double, Double) = (2.0,2.0,2.0)
          val v2: (Double, Double, Double) = (1.0,0.0,0.0)
          val v3: (Double, Double, Double) = (0.0,1.0,0.0)

          sum(v3, v2) = (1.0,1.0,0.0)
          mul(3, v1) = (6.0,6.0,6.0)

          dot(v1, v2) = 2.0
          dot(v2, v3) = 0.0
          dot(v1, v1) = 12.0

          length(v1) = 3.4641016151377544
          length(v2) = 1.0

          theta(v1, v2) = 0.9553166181245092
          theta(v3, v2) = 1.5707963267948966
          pi/2 = 1.5707963267948966

   */


//******************************************************//
object arithmetic {
  def sqrt(n: Int): Option[Int] = { // use binary search to get sqrt which would cost O(log(n)) time
    // better than linear time
    var result = 0
    var left = 0
    var right = n
    n match {
      case n if n < 0 => None
      case n if n == 0 => Some(0)
      case n if n == 1 => Some(0)
      case _ => {
        breakable {
          while (left < right) {
            val temp = (left + right) / 2
            val temp1 = temp * temp
            if (temp1 > n)
              right = temp
            else if (temp1 < n) {
              left = temp + 1
              result = temp
            } else {
              return Some(temp)
            }
          }
        }
        Some(result)
      }
    }
  }

  def log(n: Int): Option[Int] = {
    var m = 0
    var temp = 1
    n match {
      case n if n <= 0 => None
      case _ => {
        while (temp * 2 <= n) {
          temp = temp * 2
          m += 1
        }
        Some(m)
      }
    }
  }

  def isPrime(n: Int): Option[Boolean] = {
    if (sqrt(n) == None)
      None
    // use O(log(n)) time to find the value of sqrt
    var temp = sqrt(n).get // find the value where to stop
    // By doing this way, it will save about sqrt(n) time but the upper bound
    // is still linear when n is sufficiently large which is O(n).
    var isPrime = true
    breakable {
      while (temp > 2) {
        if (n % temp == 0) {
          isPrime =false
          break
        }
        temp = temp - 1
      }
    }
    Some(isPrime)
  }

  def gcd(n: Int, m: Int): Option[Int] = {
    (m, n) match {
      case (m, n) if m < 0 || n < 0 => None
      case _ => {
        var smallVal = if (n > m) m - 1 else n - 1
        for (i <- smallVal to 1 by -1 if m % smallVal != 0 || n % smallVal != 0) {
          smallVal -= 1
        }
        Some(smallVal)
      }
    }
  }

  def lcm(n: Int, m: Int): Option[Int] = {
    (n, m) match {
      case (n, m) if n < 0 || m < 0 => None
      case _ => {
        Some((n / gcd(n, m).get) * m)
      }
    }
  }

  def phi(n:Int): Option[Int] = {
    var res = 0
    n match {
      case n if n < 0 =>None
      case _=>{
        for( i <- 1 until n) {
          if (gcd(i,n).get == 1) res += 1
        }
        Some(res)
      }
    }
  }
}
//****************************************************//
println("gcd(15, 12) = " + arithmetic.gcd(15, 12))
println("lcm(15, 12) = " + arithmetic.lcm(15, 12))
println("gcd(13, 12) = " + arithmetic.gcd(13, 12))
println("gcd(-13, 12) = " + arithmetic.gcd(-13, 12))
println("phi(9)= " + arithmetic.phi(9))
println("sqrt(49) = " + arithmetic.sqrt(49))
println("sqrt(37) = " + arithmetic.sqrt(37))
println("sqrt(35) = " + arithmetic.sqrt(35))
println("log(64) = " + arithmetic.log(64))
println("log(130) = " + arithmetic.log(130))
println("log(9) = " + arithmetic.log(9))
println("log(0) = " + arithmetic.log(0))
println("isPrime(23) = " + arithmetic.isPrime(23))
println("isPrime(59) = " + arithmetic.isPrime(59))
println("isPrime(75) = " + arithmetic.isPrime(75))
println("lcm(-1,2) = " + arithmetic.lcm(-2,1))
//******************************//
    //OUTPUT FOR ARITHMETIC//
/*
      gcd(15, 12) = Some(3)
      lcm(15, 12) = Some(60)
      gcd(13, 12) = Some(1)
      gcd(-13, 12) = None
      phi(9)= Some(6)
      sqrt(49) = Some(7)
      sqrt(37) = Some(6)
      sqrt(35) = Some(5)
      log(64) = Some(6)
      log(130) = Some(7)
      log(9) = Some(3)
      log(0) = None
      isPrime(23) = Some(true)
      isPrime(59) = Some(true)
      isPrime(75) = Some(false)
      lcm(-1,2) = None
 */


//*****************************//
def rollDice():(Int,Int)={
     val a = Math.random() * 100
     val b = Math.random() * 100
     ((a.toInt % 6 + 1, b.toInt%6+1))
}
//*************************//
println(rollDice())
println(rollDice())
println(rollDice())
println(rollDice())
println(rollDice())

//**********************//

     //OUTPUT
/*
(1,5)
(3,4)
(1,3)
(3,1)
(2,2)
 */