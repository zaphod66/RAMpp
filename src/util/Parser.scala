package util

import scala.util.parsing.combinator._

trait Command

case class Commands(cmds: List[Command]) extends Command
case class IFcmd(cond: String, then_part: Commands, else_part: Commands) extends Command
case class WHILEcmd(cond: String, do_part: Commands) extends Command
case class WRITEcmd(outvar: String) extends Command
case class READcmd(invar: String) extends Command
case class ASSIGNMENTcmd(ass_var: String, action: String) extends Command

object RAMparser extends JavaTokenParsers {
  def commands = rep(command) ^^ { case cmds => Commands(cmds) }
  
  def command  = ifCmd | failure("unexpected symbol")
  
  def ifCmd: Parser[Command] = ("if" ~ ident ~ "=" ~ "0" ~ "then" ~ commands ~ optElse ~ "end") ^^ 
                                     { case "if" ~ id ~ "=" ~ "0" ~ "then" ~ thenpart ~ elsepart ~ "end" => 
                                       IFcmd(id,thenpart,elsepart) }
   def optElse: Parser[Commands] = opt("else"~commands) ^^ { case None => Commands(Nil)
                                                             case Some("else"~cmds) => cmds } 
}
