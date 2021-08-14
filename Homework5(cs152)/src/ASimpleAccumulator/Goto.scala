package ASimpleAccumulator

class Goto(arg:Int) extends Instructions {
  override def execute: Unit = AccumulatorTest.IP += 1
}

object Goto {
  def apply(arg: Int): Goto = new Goto(arg)
}