package util

object Evaluator {
  var ST: Map[String,Int] = Map()

  def eval(commands: List[Statement]): Unit = {
    if (!commands.isEmpty) {
      commands.head match {

        case READcmd(myvar)  => { 
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          print(myvar + " ? ")
          var x = readInt()
          if (x < 0) {
            println("***ERROR: Number cannot be negative")
             return
          } else {
            ST += (myvar -> x)
          }
        }

        case WRITEcmd(myvar) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          println(myvar + " : " + ST(myvar))
        }

        case ASSIGNMENTcmd(myvar, op) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          if (op == "++") {
            ST += (myvar -> (ST(myvar)+1))
          } else {
            ST += (myvar -> (ST(myvar)-1))  // allow negative numbers
          }
        }

        case IFcmd(myvar, thenPart, elsePart) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          if (ST(myvar) == 0) {
            eval(thenPart.cmds)
          } else {
            eval(elsePart.cmds)
          }
        }

        case WHILEcmd(myvar, doPart) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          while (ST(myvar) == 0) {
            eval(doPart.cmds)
          }
        }
        case _ => println("unkown statement: " + commands.head)
      }
      eval(commands.tail)
    }
  }
}
