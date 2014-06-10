package com.aakarshn

import com.aakarshn._
import org.scalatest.FlatSpec
import org.scalatest.Assertions._

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.syntactical._

import Evaluator._
import Syntax._

class LexSpec extends UnitSpec {

  val lexical = new LambdaLexer()
  import lexical._

  "Lexer" should "get semi-colon" in {
    val scanner = new Scanner(";;")
    val f = scanner.first
    assertResult(SpecialChar(";"),f){f}
  }

  it should "get float" in {
    val scanner = new Scanner("2.1")
    val f = scanner.first
    assertResult(FloatLit("2.1"),"["+f+"]"){f}
  }

  it should "get backslash" in {
    val scanner = new Scanner("/")
    val f = scanner.first
    assertResult(SpecialChar("/"),f){f}
  }
  it should "get arrow" in {
    val scanner = new Scanner("->")
    val f = scanner.first
    assertResult(SpecialChar("->"),f){f}
  }


  it should "get accepting scanners" in {
    val scanner  = new Scanner("true false if")
    println(scanner.first.getClass)
    assertResult(Keyword("true"),"scanner not getting first token"){scanner.first}
    assertResult(Keyword("false"),"scanner not getting second token"){scanner.rest.first}
    assertResult(Keyword("if"),"scanner not getting second token"){scanner.rest.rest.first}
  }

  it should "tag string literals" in {
    assertResult(Keyword("lambda")){ new Scanner("lambda x. true").first }
    assertResult(Identifier("xoxo")){new Scanner("lambda xoxo. true").rest.first}
    assertResult(Identifier("xoxo")){new Scanner("lambda xoxo. xoxo").rest.rest.rest.first}
  }

}


