package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] = ins match {
        case AddI => {
            checkStackSize(stack,2)
            applyInstruction(stack, _+_)
        }
        case SubI => {
            checkStackSize(stack,2)
            applyInstruction(stack, _-_)
        }
        case MultI => {
            checkStackSize(stack,2)
            applyInstruction(stack, _*_)
        }
        case DivI => {
            checkStackSize(stack,2)
            if (stack.last == 0) {
                throw new Exception("Divide by zero")
            }
            applyInstruction(stack, _/_)
        }
        case ExpI => {
            checkStackSize(stack,1)
            applyInstruction(stack, math.exp(_))
        }
        case LogI => {
            checkStackSize(stack,1)
            if (stack.last <= 0) {
                throw new Exception("Log of non positive")
            }
            applyInstruction(stack, math.log(_))
        }
        case SinI => {
            checkStackSize(stack,1)
            applyInstruction(stack, math.sin(_))
        }
        case CosI => {
            checkStackSize(stack,1)
            applyInstruction(stack, math.cos(_))
        }
        case PushI(f) => stack :+ f
        case PopI => {
            checkStackSize(stack,1)
            stack.init
        }
    }

    def checkStackSize(stack: List[Double], size: Int) = {
        if (stack.length < size) {
            throw new Exception("Stack too small")
        }
    }

    def applyInstruction(stack: List[Double], f: (Double, Double) => Double): List[Double] = {
        val (left, right) = stack.splitAt(stack.length - 2)
        left :+ f(right(0), right(1))
    }

    def applyInstruction(stack: List[Double], f: Double => Double): List[Double] = {
        val (left, right) = stack.splitAt(stack.length - 1)
        left :+ f(right(0))
    }
    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double =
        instructionList.foldLeft(List[Double]())(emulateSingleInstruction(_, _)).last

}