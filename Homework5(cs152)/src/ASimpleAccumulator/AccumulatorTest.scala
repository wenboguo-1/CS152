package ASimpleAccumulator

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
object AccumulatorTest{

    var program = List[Instructions]()
    var register = 0
    var halt = false
    var IP = 0

    def run() = {
      breakable {
        for (i <- 0 until program.size) {
            if(halt)
              break()
            else
              program(i).execute
        }

      }

    }
    override def toString: String = "result is " + register
    def main(args: Array[String]): Unit = {
      AccumulatorTest.program= List(Add(3), Mul(5), Add(1), Mul(2))
      AccumulatorTest.run()
      println(AccumulatorTest.register)                        //> res6: Int = 32
      // computing (((10 * 2) + 3) * 5)
      AccumulatorTest.register = 0
      AccumulatorTest.program = List(Add(10), Mul(2), Add(3), Mul(5))
      AccumulatorTest.run()
      println(AccumulatorTest.register)

      // 10 * 2 + 3 then stop
      AccumulatorTest.register = 0
      AccumulatorTest.program = List(Add(10), Mul(2), Add(3), Halt(), Mul(5))
      AccumulatorTest.run()
      println(AccumulatorTest.register)

    }

  /**
       OUTPUT
       32
       115
       23
   */
}


