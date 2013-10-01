package util

import scala.util.parsing.combinator._

trait Command

case class Commands(cmds: List[Command]) extends Command
case class IFcmd(cond: String, then_part: Commands, else_part: Commands) extends Command
case class WHILEcmd(cond: String, do_part: Commands) extends Command
case class WRITEcmd(outvar: String) extends Command
case class READcmd(invar: String) extends Command
case class ASSIGNMENTcmd(ass_var: String, action: String) extends Command

