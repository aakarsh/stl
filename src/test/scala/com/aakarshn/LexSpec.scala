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

class LexSpec extends UnitSpec {

  val lexical = new LambdaLexer()

  "Lexer" should "get semi-colon" in {
    val scanner = new lexical.Scanner(";;")
    val f = scanner.first
    assertResult(lexical.Semicolon,f){f}
  }

  it should "get accepting scanners" in {
    val scanner  = new lexical.Scanner("true false if")
    println(scanner.first.getClass)
    assertResult(lexical.Keyword("true"),"scanner not getting first token"){scanner.first}
    assertResult(lexical.Keyword("false"),"scanner not getting second token"){scanner.rest.first}
    assertResult(lexical.Keyword("if"),"scanner not getting second token"){scanner.rest.rest.first}
  }

  it should "tag string literals" in {
    assertResult(lexical.Keyword("lambda")){ new lexical.Scanner("lambda x. true").first }
    assertResult(lexical.Identifier("xoxo")){new lexical.Scanner("lambda xoxo. true").rest.first}
    assertResult(lexical.Identifier("xoxo")){new lexical.Scanner("lambda xoxo. xoxo").rest.rest.rest.first}
  }

}


