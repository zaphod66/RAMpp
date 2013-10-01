package util

object Evaluator {
  var ST: Map[String,Int] = Map()

  def eval(commands: List[Command]): Unit = {
    if (!commands.isEmpty) {
      commands.head match {
        case READcmd(myvar)  => if (! ST.contains(myvar) ) {
                                 ST += (myvar -> 0)
                               }
                               print("? ")
                               var x = readInt()
                               if (x < 0) {
                                 println("***ERROR: Number cannot be negative")
                                 return
                               }
                               else
                                 ST += (myvar -> x)

        case WRITEcmd(myvar) => if (! ST.contains(myvar) ) {
                                  ST += (myvar -> 0)
                                }
                                println(ST(myvar))
        case _ => 
      }
      eval(commands.tail)
    }
  }
}
