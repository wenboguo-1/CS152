package time2

class Time2(var hour: Int,  private var minue1: Int = 0) {

  if (hour < 0 || 23 < hour || minue1 < 0 || 59 < minue1)
    throw new IllegalArgumentException

   def minute_=(newTime: Int) = {
     if (newTime < 0 || 59 < newTime)
       throw new IllegalArgumentException
    minue1 = newTime
  }

  def minute = minue1

  def minutesSinceMidNight = hour * 60 + minue1

  override def toString: String = hour.toString + ":" + minue1.toString

  def before(other: Time2) = this.minutesSinceMidNight < other.minutesSinceMidNight
}

object Time2 {
  def apply(hour: Int, minute: Int = 0) = new Time2(hour, minute)
}
