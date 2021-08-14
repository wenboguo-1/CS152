package Queue

import scala.collection.mutable.ArrayBuffer


class Queue[T](elems:T*) {
     private var headIndex = 0;
     private val capacity = elems.size
     private var queue: ArrayBuffer[T] = new ArrayBuffer[T](capacity)
     private var count = capacity

     def setQueue(): Unit ={
        for(ele <- elems)
           queue += ele
     }
     def isEmpty(): Boolean ={
        if(count == 0)
          true
        else
          false
     }

     def enQueue(ele: T) = {
          if(capacity  == count){
            print("Capacity is full, can not add anything")
          }else{
           queue.insert((count + capacity) % capacity,ele)
            count += 1
          }
     }
     def deQueue() = {
         if(this.count == 0){
           print("The queue is already empty, can remove anything")
         }else{
             headIndex = (headIndex+1)%capacity
             count -= 1
         }
     }
     def front():T =  {
         queue(headIndex)
    }

  override def toString: String = "The front value is " + front()

}

object Queue {
  def apply[T](elems: T*) ={

    new Queue(elems: _*)
  }
  def test1() {
    val waitingList = Queue[String]("Sid", "Barb", "Joel","gaga","gugu")
     waitingList.setQueue()
     while(!waitingList.isEmpty()){
        println (waitingList)
        waitingList.deQueue()
     }
  }
}
