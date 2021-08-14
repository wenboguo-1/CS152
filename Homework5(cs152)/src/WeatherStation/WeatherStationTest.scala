package WeatherStation


object WeatherStationTest extends App {
  val thermometer: IThermometer = new ThermAdapter
  val avgTemp =
    thermometer.getMeanTemperature(List("LA", "SF", "SLC", "Rio"))
    println("avg temp = " + avgTemp)
}

/**
    output
 avg temp = 72.55356747238469
 avg temp = 64.46813130965238
 avg temp = 76.10690574652936
 */