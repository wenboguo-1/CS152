package time1

object TestTime1 extends App {
  try {
    val t1 = new Time(3, 45)
    val t2 = new Time(3)

    println("t1 = " + t1.toString)
    println("t2 = " + t2.toString)
    println("t1 < t2 = " + t1.before(t2))
    println("t1 minutes since midnight = " + t1.minutesSinceMidNight)
    val t3 = Time(14, 60) // oops!
  } catch {
    case e: IllegalArgumentException => println(e)
  }

}
/**
    output
    t1 = 3:45
    t2 = 3:0
    t1 < t2 = false
    t1 minutes since midnight = 225
    java.lang.IllegalArgumentException
 */