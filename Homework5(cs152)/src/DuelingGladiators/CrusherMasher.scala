package DuelingGladiators

class CrusherMasher extends Gladiator("CrusherMasher") with Crusher with Masher {

    override def attack(opponent: Gladiator): Unit = {
     super.attack(opponent)
      super.mash(opponent)
      super.crush(opponent)
  }
}
object CrusherMasher{
  def apply(): CrusherMasher = new CrusherMasher()
}
