package com.aakarshn

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input.CharArrayReader.EofCh

import scala.io.Source
import java.io._


// Mostly Ripped off from JSON Lexer
class LambdaLexer extends StdLexical with ImplicitConversions { 

  reserved ++= List("true", "false","if","then","else","iszero","succ","pred","lambda","let")
  delimiters ++= List("{", "}", "[", "]", ":", ",","(",")",".")

  override def token: Parser[Token] =
    (
      identChar ~ rep( identChar | digit )^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character")
    )

  def checkKeyword(xs : List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  def number = intPart ~ opt(fracPart) ~ opt(expPart) ^^ { case i ~ f ~ e =>
    i + optString(".", f) + optString("", e)
  }
  def intPart = zero | intList
  def intList = nonzero ~ rep(digit) ^^ {case x ~ y => (x :: y) mkString ""}
  def fracPart = '.' ~> rep(digit) ^^ { _ mkString "" }
  def expPart = exponent ~ opt(sign) ~ rep1(digit) ^^ { case e ~ s ~ d =>
    e + optString("", s) + d.mkString("")
  }

  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  /** A string is a collection of zero or more Unicode characters, wrapped in
   *  double quotes, using backslash escapes (cf. http://www.json.org/).
   */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespace = rep(whitespaceChar)

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
    |'\\' ~ '\\' ^^^ "\\"
    |'\\' ~ '/'  ^^^ "/"
    |'\\' ~ 'b'  ^^^ "\b"
    |'\\' ~ 'f'  ^^^ "\f"
    |'\\' ~ 'n'  ^^^ "\n"
    |'\\' ~ 'r'  ^^^ "\r"
    |'\\' ~ 't'  ^^^ "\t"
    |'\\' ~> 'u' ~> unicodeBlock)

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

      /*
  def SPACE:Parser[String] = " ".r
  def ZERO:Parser[String] = "0"
  def TRUE:Parser[String] = "true"
  def FALSE:Parser[String] = "false"
  def ID:Parser[String] =  "[a-z][A-Z0-9]*".r
  def LAMBDA:Parser[String] =  "lambda".r
  def DOT:Parser[String] ="."
  def IF:Parser[String] ="if".r
  def THEN:Parser[String] ="then".r
  def ELSE:Parser[String] ="else".r
  def ISZERO:Parser[String] ="iszero".r
  def SUCC:Parser[String] ="succ".r
  def PRED:Parser[String] ="pred".r
  def LPAREN:Parser[String] ="("
  def RPAREN:Parser[String] =")"
    */

}
