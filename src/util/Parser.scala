package util

import scala.util.parsing.combinator._

trait Statement

case class Statements(cmds: List[Statement]) extends Statement
case class IFcmd(cond: String, then_part: Statements, else_part: Statements) extends Statement
case class WHILEcmd(cond: String, do_part: Statements) extends Statement
case class WRITEcmd(outvar: String) extends Statement
case class READcmd(invar: String) extends Statement
case class ASSIGNMENTcmd(ass_var: String, action: String) extends Statement

object RAMparser extends JavaTokenParsers {
  def commands = rep(command) ^^ { case cmds => Statements(cmds) }

  def command  = ifCmd | readCmd | writeCmd | assignCmd | whileCmd | failure("unexpected symbol")

  def readCmd =  ("read" ~ ident)  ^^ { case "read"~id  => READcmd(id) }

  def writeCmd = ("write" ~ ident) ^^ { case "write"~id => WRITEcmd(id) }

  def assignCmd = (ident ~ ("++" | "--")) ^^ { case id ~ op => ASSIGNMENTcmd(id, op) }

  def ifCmd: Parser[Statement] = ("if" ~ ident ~ "=" ~ "0" ~ "then" ~ commands ~ optElse ~ "end") ^^ 
                                     { case "if" ~ id ~ "=" ~ "0" ~ "then" ~ thenpart ~ elsepart ~ "end" => 
                                       IFcmd(id, thenpart, elsepart) }

  def optElse: Parser[Statements] = opt("else"~commands) ^^ { case None => Statements(Nil)
                                                            case Some("else" ~ cmds) => cmds
                                                            case Some(_) => Statements(Nil) } // just to make the match exhaustive 

  def whileCmd: Parser[Statement] = ("while" ~ ident ~ "=" ~ "0" ~ "do" ~ commands ~ "end") ^^
                                  { case "while" ~ id ~ "=" ~ "0" ~ "do" ~ cmds ~ "end" => WHILEcmd(id, cmds) }
}
