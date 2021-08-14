package stackMachine
import collection.mutable.{ListBuffer, Stack}
class Pop extends Command {
  override def command(s: ListBuffer[Int]): Unit =  s -= s(0)
}
object  Pop{
  def apply(): Pop = new Pop()
}
