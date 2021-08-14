package WeatherStation

class CelsiusTherm {
  // = degrees Celsius
  def computeTemp(city: String) = 50 * math.random // fake temperature for now
}
object CelsiusTherm {
  def apply(): CelsiusTherm = new CelsiusTherm()
}
