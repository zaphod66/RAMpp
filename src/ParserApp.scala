import java.io.FileReader

import util._   // Parser, Evaluator

object ParserApp extends App {

  println("ParserApp")
  
  val reader = new FileReader("RAM.src")
  val result = RAMparser.parseAll(RAMparser.commands, reader)
  if (result.successful) {
    val parseTree = result.get
    Evaluator.eval(parseTree.cmds)
  } else {
    println(result)
  }
}
