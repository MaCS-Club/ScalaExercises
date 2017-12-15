import org.scalatest.FlatSpec
class Automate extends FlatSpec {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)
  
  def simulateMachine(inputs: List[Input]): FuncState.State[Machine, (Int, Int)]= ???
  val inputCoin = List(Coin)
  val inputTurn = List(Turn)  

  val machine1 = Machine(true, 1, 0)
  val machine2 = Machine(false, 1, 1)
  val m2Result = simulateMachine(inputTurn).run(machine2)
  val machine3 = Machine(true, 0, 1)
  
  "Inserting a coin into a locked machine" should "cause it to unlock if there’s any candy left" in {
     assert(!simulateMachine(inputCoin).run(machine1)._2.locked)
  }
  "Turning the knob on an unlocked machine" should "cause it to dispense candy and become locked." in {
     assert((m2Result._2.locked) &&
     (m2Result._2.candies == 0))
  }
  "Turning the knob on a locked machine or inserting a coin into an unlocked machine" should " not do anything." in {
     assert((simulateMachine(inputTurn).run(machine1)._2.locked == machine1.locked) &&
     (simulateMachine(inputCoin).run(machine2)._2.locked == machine2.locked))
   }
  "A machine that’s out of candy" should  "ignore all inputs." in {
    assert((simulateMachine(inputTurn).run(machine3)._2.locked == machine3.locked) &&
    (simulateMachine(inputCoin).run(machine3)._2.locked == machine3.locked))
  }
}
