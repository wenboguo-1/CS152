package value

class Notification(message:String) extends Value {
  override def toString: String = this.message
}
object Notification{
  def apply(n:String): Notification = new Notification(n)
  val ok = Notification("oK")
  val done = Notification("Done")
  val unspecified = Notification("UNSPECIFIED")
}
