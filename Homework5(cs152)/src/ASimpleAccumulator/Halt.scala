package ASimpleAccumulator

class Halt extends Instructions {
  override def execute: Unit = AccumulatorTest.halt = true
}
object Halt{
  def apply(): Halt = new Halt()
}
