package stackMachine
import scala.collection.mutable.{ListBuffer, Stack}
class Sum extends Command {
  override def command(s: ListBuffer[Int]): Unit = {
    if(s.size >= 2) s.update(s.size-1,s(s.size-1) + s(s.size-2))
  }
  
}
object Sum{
  def apply(): Sum = new Sum()
}
