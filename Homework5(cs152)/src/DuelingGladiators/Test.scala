package DuelingGladiators

object Test {
  val a = BumbleBee
  val b = CrusherMasher()
  def main(args: Array[String]): Unit = {
    for( i <- 0 to 5){
      if(b.getHealth() > 0 && a.getHealth() >0)
        a.attack(b)
      if(b.getHealth() > 0 && a.getHealth() >0)
        b.attack(a)

    }
  }

}
/**
        OUTPUT
BumbleBee is attacking CrusherMasher
CrusherMasher's Health = 100

CrusherMasher is attacking BumbleBee
BumbleBee's Health = 90

BumbleBee is attacking CrusherMasher
CrusherMasher's Health = 85

CrusherMasher is attacking BumbleBee
BumbleBee's Health = 1

(CrusherMasher, is killed already )
 */