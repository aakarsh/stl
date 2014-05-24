import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

package com.aakarshn {

  object Evaluator  {

    abstract class EvalException extends Throwable

    case class NoRulesApply(s:String) extends  EvalException

    abstract class Term
    case class Unit extends Term
    case class True extends Term
    case class False extends Term
    case class Zero extends Term
    case class Succ(t:Term) extends Term
    case class Pred(t:Term) extends Term
    case class If(t1:Term,t2:Term,t3:Term) extends Term
    case class IsZero(t:Term) extends Term

    def is_numerical(t:Term): Boolean = {
      t match {
        case Zero() => true
        case Succ(t) => is_numerical(t)
        case Pred(t) => is_numerical(t)
        case _ => false
      }
    }

    def is_value(t:Term): Boolean = {
      t match {
        case True() => true
        case False() => true
        case t if is_numerical(t) => true
        case _ => false
      }
    }

    class LCParser extends RegexParsers {

      def value:Parser[Term] = (
        "0".r^^{_=>  Zero() }      |
        "true".r^^{_=>  True()}   |
        "false".r^^{_=> False()}
      )

      def expr:Parser[List[Term]] = repsep(term,";") |repsep(term,"\n")

      /**
      def expr:Parser[Term] = term ~rep("\n".r~term |";".r~term) match {
        case (t~rest) => rest.foldLeft
      }
      */

      def term:Parser[Term] = (
        value                                      |
          """iszero""".r~term  ^^ {
            case("iszero"~v) => IsZero(v)
          }  |
          "if".r~term~"then".r~term~"else".r~term ^^ { s => s match {
            case("if"~t1~"then"~t2~"else"~t3) => If(t1,t2,t3)
          }}  |
          "succ".r~term ^^ {
            case("succ"~v) =>
              Succ(v)
          }                                         |
          "pred".r~term ^^ {
            case("pred"~v) =>
              Pred(v)
          }
      )

      def expression_parser = expr

      def fromReader(r:Reader):List[Term] = {
        parse(expr,r)  match  {
          case Success(result,_) => result
          case f: NoSuccess => scala.sys.error(f.msg)
        }
      }

      def fromString(s:String):List[Term] =
         parseAll(expr,s) match  {
          case Success(result,_) => result
          case f: NoSuccess => scala.sys.error(f.msg)
        }
    }

    def eval1(term: Term):Term = {

      def eval_numerical(t1:Term) = {
        val te = eval1(t1)
        require(is_numerical(te))
        te
      }

      term match {
        case  If(True(),t2,t3) => t2
        case  If(False(),t2,t3) => t3
        case  If(t1:Term,t2:Term,t3:Term)   => If(eval1(t1),t2,t3)
        case  Succ(Pred(t1)) => t1
        case  Succ(t1)  =>  Succ(eval_numerical(t1))
        case  Pred(Succ(t1)) => t1
        case  Pred(t1)  => Pred(eval_numerical(t1))
        case  IsZero(Zero()) => True()
        case  IsZero(Succ(t:Term)) => False()
        case  IsZero(Pred(t:Term)) => False()
        case  IsZero(t1) => IsZero(eval1(t1))
        case _ => throw NoRulesApply("Out of rules")
      }
    }

    def eval(term:Term):Term = {
      try {
        eval1(term)
      } catch{
        case ex:NoRulesApply => term
      }
    }

    def eval(terms:List[Term]): List[Term] = {
      terms.map(eval _);
    }


    def parse(s:String):List[Term] =  new LCParser().fromString(s)
    def parse1(s:String):Term =  new LCParser().fromString(s)(0)


    def run(prog: String) = {
      val t = parse(prog)
      t.map(eval _)
    }

    def run(reader:Reader) = {
      val parsed_result = new LCParser().fromReader(reader)
      parsed_result.map(eval _);      
    }

    def run1(prog: String) = {
      run(prog)(0)
    }

    def print_results(terms:List[Term]):scala.Unit = terms.map(print_result)

    def print_result(prog:Term):scala.Unit = {
      prog match {
        case False() => println("false")
        case True() => println("true")
        case Zero() => println("0")
        case  Succ(t1:Term)  =>  {
          print("succ ");
          print_result(t1)
        }
        case  Pred(t1:Term)  =>  {
          print("pred ");
          print_result(t1)
        }
        case _ => throw NoRulesApply("Out of rules")
      }
    }

    def main(args:Array[String]):scala.Unit= {
      run_assertions()

      if(args.length < 2) {
        println("Usage: stl <input-file>")
        return;
      }

      val reader  = new FileReader(args(1));
      val results =run(reader);
      results.map(pr => {
        val rs = eval(pr)
        print_result(rs);
      })
    }

    def run_assertions(): scala.Unit = {

    // Begin Assertions here

      //numerical tests
      require(is_numerical(Zero()))
      require(is_numerical(Succ(Zero())))
      require(is_numerical(Pred(Zero())))

      //value tests
      require(is_value(True()))
      require(is_value(False()))
      require(is_value(Succ(Zero())))
      require(is_value(Pred(Zero())))

      // Evaluator tests
      require(True() == eval(If(True(),True(),False())), "If did not evaluate true correctly")
      require(False() == eval(If(False(),True(),False())), "If did not evaluate false correctly")
      require(Succ(Zero()) == eval(Succ(Zero())), "succ evaluated incorrectly")
      require(Succ(Zero()) == eval(Succ(If(False(),Succ(Zero()),Zero()))), "succ evaluated incorrectly")
      require(True() == eval(IsZero(Zero())), "iszero is not evaluting correctly")
      require(False() == eval(IsZero(Succ(Zero()))), "iszero is not evaluting correctly")

      require(True() == parse1("true"), "parsing atomic true not working")
      require(Succ(Zero()) == parse1("succ 0"), "parsing atomic true not working")

      require(Zero() == run1("succ pred 0"), "succ not working with pred")
      require(Zero() == run1("pred succ 0"), "pred not working with succ")

      require(List(True(),False()) == run("true;false\n"),"multi expression parsing not working")

      require(Zero() == run1("if true then 0 else succ 0"), "if-true evaluation not working")
      require(Succ(Zero()) == run1("if false then 0 else succ 0"),"if-false  evaluation not working")

      println("All assertions passed !")
    }
  }

}

import com.aakarshn._

