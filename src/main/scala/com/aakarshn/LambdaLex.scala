package com.aakarshn

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input.CharArrayReader.EofCh

import scala.io.Source
import java.io._


// Mostly Ripped off from JSON Lexer
class LambdaLexer extends StdLexical with ImplicitConversions { 

  val types =List("Bool","Nat","Unit","String","Float")
  val nat = List("iszero","succ","pred")
  val if_k = List("if","then","else")
  val base_vals = List("true","false")

  reserved ++= types:::nat:::if_k:::base_vals:::List("lambda","let","in")

  delimiters ++= List("{", "}", "[", "]", ":", ",","(",")",".")

  val special_chars = List(';',':','=','/')

  override def token: Parser[Token] =
    (   identChar ~ rep( identChar | digit )^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | special_char
      | delim
      | '\"' ~> failure("Unterminated string")
//      | '/' ^^^ SpecialChar('/')
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character"))

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

  def special_char = elem("special_char", {special_chars.contains(_)})^^{SpecialChar(_)}

  case class SpecialChar(char: Char) extends Token {
    def chars = char.toString
    override def toString = chars
  }

}
