package DuelingGladiators

trait Crusher {

     def crush(gladiator: Gladiator) = {
         gladiator.decHealth(5)
     }
}
