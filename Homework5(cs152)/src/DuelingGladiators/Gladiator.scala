package DuelingGladiators

class Gladiator(val name: String, private var health: Int = 100) {

      def getHealth() = health
      def decHealth(damage:Int):Unit = {
        this.health -= damage
        if(this.getHealth() < 0 ){
          print(this.name, " is killed already ")
        }

      }
      def attack(opponent: Gladiator) ={
            if(this.getHealth() > 0 && opponent.getHealth() > 0) {
              println(this.name + " is attacking " + opponent.name)
              println( opponent.name + "'s Health = " + health)
              println()
              decHealth(scala.util.Random.nextInt(opponent.health))
            }
         }

}
object Gladiator {
  def apply(name: String, health: Int): Gladiator = new Gladiator(name, health)
}