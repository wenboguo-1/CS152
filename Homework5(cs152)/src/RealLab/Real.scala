package RealLab

class Real(val scalaValue: Double) extends Ordered[Real] with Equals {


  def *(that:Real) = this.scalaValue * that.scalaValue
  override def <(that: Real): Boolean = this.scalaValue < that.scalaValue
  override def compare(that:Real) = {
        this.scalaValue.compare(that.scalaValue)
    }
  override def equals(obj: Any): Boolean = {
    obj match {
      case r: Real =>
        r.canEqual(this) && r.scalaValue == this.scalaValue
      case _ => false
    }
  }
  override def canEqual(that: Any): Boolean = {
       that.isInstanceOf[Real]
  }
}

object Real{
  def apply(scalaValue: Double): Real = new Real(scalaValue)
}

