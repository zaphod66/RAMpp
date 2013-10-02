package util

import scala.util.parsing.combinator._

trait Statement

case class Statements(cmds: List[Statement]) extends Statement
case class IFstmt(cond: String, value: Int, then_part: Statements, else_part: Statements) extends Statement
case class WHILEstmt(cond: String, value: Int, do_part: Statements) extends Statement
case class WRITEstmt(outvar: String) extends Statement
case class READstmt(invar: String) extends Statement
case class INCDECstmt(incvar: String, action: String) extends Statement
case class ASSIGNstmt(assvar: String, value: Int) extends Statement

object RAMparser extends JavaTokenParsers {
  def commands = rep(command) ^^ { case cmds => Statements(cmds) }

  def command  = ifCmd | readCmd | writeCmd | incdecCmd | whileCmd | assignCmd | failure("unexpected symbol")

  def readCmd =  ("read" ~ ident)  ^^ { case "read"~id  => READstmt(id) }

  def writeCmd = ("write" ~ ident) ^^ { case "write"~id => WRITEstmt(id) }

  def incdecCmd = (ident ~ ("++" | "--")) ^^ { case id ~ op => INCDECstmt(id, op) }

  def ifCmd: Parser[Statement] = ("if" ~ ident ~ "=" ~ wholeNumber ~ "then" ~ commands ~ optElse ~ "end") ^^ 
                                 { case "if" ~ id ~ "=" ~ value ~ "then" ~ thenpart ~ elsepart ~ "end" => 
                                       IFstmt(id, value.toInt, thenpart, elsepart) }

  def optElse: Parser[Statements] = opt("else" ~ commands) ^^ { case None => Statements(Nil)
                                                                case Some("else" ~ cmds) => cmds
                                                                case Some(_) => Statements(Nil) } // just to make the match exhaustive 

  def whileCmd: Parser[Statement] = ("while" ~ ident ~ "=" ~ wholeNumber ~ "do" ~ commands ~ "end") ^^
                                    { case "while" ~ id ~ "=" ~ value ~ "do" ~ cmds ~ "end" => WHILEstmt(id, value.toInt,  cmds) }

  def assignCmd: Parser[Statement] = ("set" ~ ident ~ "=" ~ wholeNumber) ^^
                                     { case "set" ~ id ~ "=" ~ value => ASSIGNstmt(id, value.toInt) }
}
