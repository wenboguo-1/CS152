package stackMachine
import scala.collection.mutable.ListBuffer

class Top extends Command {
  override def command(s: ListBuffer[Int]): Unit = print("Top value is " + s(s.size-1))
}
object Top{
  def apply(): Top = new Top()
}
