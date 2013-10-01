package util

object Evaluator {
  def eval(commands: List[Command]): Unit = {
    if (!commands.isEmpty) {
      commands.head match {
        case _ => 
      }
      eval(commands.tail)
    }
  }
}
