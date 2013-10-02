package util

object Evaluator {
  var ST: Map[String,Int] = Map()

  def eval(commands: List[Statement]): Unit = {
    if (!commands.isEmpty) {
      commands.head match {

        case READstmt(myvar)  => { 
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

        case WRITEstmt(myvar) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          println(myvar + " : " + ST(myvar))
        }

        case INCDECstmt(myvar, op) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          if (op == "++") {
            ST += (myvar -> (ST(myvar)+1))
          } else {
            ST += (myvar -> (ST(myvar)-1))  // allow negative numbers
          }
        }

        case IFstmt(myvar, value, thenPart, elsePart) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          if (ST(myvar) == value) {
            eval(thenPart.cmds)
          } else {
            eval(elsePart.cmds)
          }
        }

        case WHILEstmt(myvar, value, doPart) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          while (ST(myvar) == value) {
            eval(doPart.cmds)
          }
        }

        case ASSIGNstmt(myvar, value) => {
          if (! ST.contains(myvar)) { ST += (myvar -> 0) }
          ST += (myvar -> value)
        }

        case _ => println("unkown statement: " + commands.head)
      }
      eval(commands.tail)
    }
  }
}
