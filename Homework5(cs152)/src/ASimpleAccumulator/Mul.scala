package ASimpleAccumulator

class Mul(val arg:Int) extends Instructions {
  override def execute: Unit = AccumulatorTest.register *= arg
}

object Mul{

  def apply(arg: Int): Mul = new Mul(arg)
}
