package stackMachine
import ASimpleAccumulator.Instructions

import scala.collection.mutable.{ListBuffer, Stack}
object StackMachineTest extends App{
    var stack = new ListBuffer[Int]()

    var program =  List[Command]()
    def run(): Unit = {
         for(i <- 0 until  program.size){
            program(i).command(stack)
         }
    }

    StackMachineTest.program = List(Push(3), Push(4), Push(5), Sum(), Times(), Top())
    StackMachineTest.run()                             //> top = 36
    println()
    StackMachineTest.program = List(Push(10), Push(10), Times(), Push(20), Sum(), Top())
    StackMachineTest.run()

}

/**
 *  OUTPUT
 * Top value is 36
 * Top value is 120
 *
 */