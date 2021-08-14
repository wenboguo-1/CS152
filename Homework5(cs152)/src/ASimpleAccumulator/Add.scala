package ASimpleAccumulator

class Add(val arg:Int) extends Instructions {
  override def execute: Unit = AccumulatorTest.register += arg
}
object Add {

  def apply(arg: Int): Add = new Add(arg)
}
