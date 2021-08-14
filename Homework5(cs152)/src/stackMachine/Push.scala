package stackMachine
import scala.collection.mutable.{ListBuffer, Stack}

class Push(val arg: Int)extends Command {
  override def command(s: ListBuffer[Int]): Unit = {
    s += arg
  }
}
object Push{
  def apply(arg:Int): Push = new Push(arg)
}