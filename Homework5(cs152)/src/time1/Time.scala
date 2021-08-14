package time1

class Time(val hour:Int, private val _mins:Int = 0) {
   if(hour < 0 || hour >= 24 || _mins< 0 || _mins >= 60){
     throw new IllegalArgumentException
   }
   def minutesSinceMidNight() = {hour*60+_mins  }
   def before(time2:Time):Boolean =  this.minutesSinceMidNight() < time2.minutesSinceMidNight()
   override def toString = hour + ":" + _mins
}
object Time{

  def apply(hour:Int,mins:Int = 0): Unit ={
      new Time(hour,mins)
  }
}