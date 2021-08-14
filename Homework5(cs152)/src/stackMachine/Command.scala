package stackMachine

import scala.collection.mutable.ListBuffer

trait Command {
    def command(s:ListBuffer[Int]):Unit
}
