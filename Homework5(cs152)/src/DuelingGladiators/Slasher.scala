package DuelingGladiators

trait Slasher {
     def slash(gladiator: Gladiator): Unit ={
         gladiator.decHealth(5)
     }
}
