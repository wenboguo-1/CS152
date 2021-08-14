package stackMachine
import scala.collection.mutable.ListBuffer

class Times extends Command {
  override def command(s: ListBuffer[Int]): Unit = if(s.size >=2 ) s.update(s.size-1,s(s.size-1) * s(s.size-2))
}
object Times{
  def apply(): Times = new Times()
}