package edu.colorado.csci3155.project1

object StackMachineCompiler {

    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = e match {
        case Const(f) => List(PushI(f))
        case Plus(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) :+ AddI
        case Minus(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) :+ SubI
        case Mult(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) :+ MultI
        case Div(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) :+ DivI
        case Exp(e) => compileToStackMachineCode(e) :+ ExpI
        case Log(e) => compileToStackMachineCode(e) :+ LogI
        case Sine(e) => compileToStackMachineCode(e) :+ SinI
        case Cosine(e) => compileToStackMachineCode(e) :+ CosI
    }
}
