package Day10

import utils.AoCUtils

object day10 {

  trait Instruction {
    def applyToRegsiter(registerVal: Int): Int
    val cycleTime: Int
  }
  object Noop extends Instruction {
    override val cycleTime: Int = 1
    override def applyToRegsiter(registerVal: Int): Int = registerVal
  }
  case class Add(v: Int) extends Instruction {
    def applyToRegsiter(registerVal: Int): Int = registerVal + v // Kind of exteps Int + Int to be definedd
    val cycleTime: Int = 2
  }

  case class Sprite(pos: Int)
  case class State(currentVal:Int , cycle: Int)
  case class StateMachine(initialState: State, states: List[State]) {
    def getSignalStrengthAtCycle(cycle: Int): Int = {
      getValueAtCycle(cycle) * cycle
    }
    def getValueAtCycle(cycle: Int): Int = {
      states.takeWhile(state => state.cycle < cycle).reverse.head.currentVal
    }

  }
  object StateMachine {

  }


  def main(args: Array[String]): Unit = {
    //val lines = AoCUtils.readDemoFile(10)
    val lines = AoCUtils.readInputFile(10)
    val instructions = lines.map(l => {
      if (l == "noop") Noop
      else {
        val parts = l.split(" ")
        Add(parts(1).toInt)
      }
    })

    val initialState = State(1,0)
    def rec(remainingInstructions: List[Instruction], accumulatedStates: List[State]): List[State] = {
      if (remainingInstructions.isEmpty)
        accumulatedStates
      else {
        val curState = accumulatedStates.head
        val instruction = remainingInstructions.head
        val newVal  = instruction.applyToRegsiter(curState.currentVal)
        val newState = State(newVal, curState.cycle + instruction.cycleTime)
        rec(remainingInstructions.tail, newState :: accumulatedStates)
      }
    }
    val states = rec(instructions, List(initialState)).reverse
    val stateMachine = StateMachine(initialState, states)

    /*
    println("Result = " + (
        stateMachine.getSignalStrengthAtCycle(20) +
        stateMachine.getSignalStrengthAtCycle(60) +
        stateMachine.getSignalStrengthAtCycle(100) +
        stateMachine.getSignalStrengthAtCycle(140) +
        stateMachine.getSignalStrengthAtCycle(180) +
          stateMachine.getSignalStrengthAtCycle(220)
    ))


    println("Value during 20 cycle: " + stateMachine.getSignalStrengthAtCycle(20))
    println("Value during 60 cycle: " + stateMachine.getSignalStrengthAtCycle(60))
    println("Value during 100 cycle: " + stateMachine.getSignalStrengthAtCycle(100))
    println("Value during 140 cycle: " + stateMachine.getSignalStrengthAtCycle(140))
    println("Value during 180 cycle: " + stateMachine.getSignalStrengthAtCycle(180))
    println("Value during 220 cycle: " + stateMachine.getSignalStrengthAtCycle(220))
    //println(instructions)
     */
    val pixelsPerLine = 40
    val litPixels = (1 to 240 ).toList.map(cycleNum => {
      // Get sprite on cycle number
      val pixelNum = cycleNum % pixelsPerLine - 1
      val spritePos = stateMachine.getValueAtCycle(cycleNum)
      // Ex: Cycle: 1, Pixel: 0 SpritePos = 1 => -1 <= 0 <= 1
      (spritePos - 1) <= pixelNum  && pixelNum <= (spritePos + 1)
    })
    printPixels(litPixels)
  }

  def printPixels(pixels: List[Boolean]): Unit = {
    val lines = pixels.grouped(40).toList

    lines.foreach(linePixels => {
      val line = linePixels.map(isLit => {
        if (isLit) " # " else "   "
      })
      println(line.mkString)
    })


  }


}
