package WeatherStation

class ThermAdapter extends IThermometer {

     override def getMeanTemperature(cities :List[String]) = {
         cities.map((f:String )=> 1.8 * (50 *  math.random) + 32 ).reduce((x:Double, y:Double) => x + y ) / cities.size
  }
}
