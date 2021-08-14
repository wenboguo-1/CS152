package DuelingGladiators

object BumbleBee extends Gladiator("BumbleBee") with Slasher with  Masher {
  override def attack(opponent: Gladiator): Unit = {
      super.attack(opponent)
      super.mash(opponent)
      super.slash(opponent)

  }
}
