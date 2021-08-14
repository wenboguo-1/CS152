package DuelingGladiators

trait Masher {
     def mash(gladiator: Gladiator): Unit ={
       gladiator.decHealth(5)
     }
}
