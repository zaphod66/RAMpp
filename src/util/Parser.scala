package util

import scala.util.parsing.combinator._

trait Statement

case class Statements(cmds: List[Statement]) extends Statement
case class IFstmt(cond: String, then_part: Statements, else_part: Statements) extends Statement
case class WHILEstmt(cond: String, do_part: Statements) extends Statement
case class WRITEstmt(outvar: String) extends Statement
case class READstmt(invar: String) extends Statement
case class INCDECstmt(ass_var: String, action: String) extends Statement

object RAMparser extends JavaTokenParsers {
  def commands = rep(command) ^^ { case cmds => Statements(cmds) }

  def command  = ifCmd | readCmd | writeCmd | assignCmd | whileCmd | failure("unexpected symbol")

  def readCmd =  ("read" ~ ident)  ^^ { case "read"~id  => READstmt(id) }

  def writeCmd = ("write" ~ ident) ^^ { case "write"~id => WRITEstmt(id) }

  def assignCmd = (ident ~ ("++" | "--")) ^^ { case id ~ op => INCDECstmt(id, op) }

  def ifCmd: Parser[Statement] = ("if" ~ ident ~ "=" ~ "0" ~ "then" ~ commands ~ optElse ~ "end") ^^ 
                                     { case "if" ~ id ~ "=" ~ "0" ~ "then" ~ thenpart ~ elsepart ~ "end" => 
                                       IFstmt(id, thenpart, elsepart) }

  def optElse: Parser[Statements] = opt("else"~commands) ^^ { case None => Statements(Nil)
                                                            case Some("else" ~ cmds) => cmds
                                                            case Some(_) => Statements(Nil) } // just to make the match exhaustive 

  def whileCmd: Parser[Statement] = ("while" ~ ident ~ "=" ~ "0" ~ "do" ~ commands ~ "end") ^^
                                  { case "while" ~ id ~ "=" ~ "0" ~ "do" ~ cmds ~ "end" => WHILEstmt(id, cmds) }
}
