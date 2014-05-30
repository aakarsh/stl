import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

package com.aakarshn {

  /**
    A direct port of the evaluator for simply typed lambda calculus.    
    */
  object Evaluator  {

    abstract class EvalException extends Throwable

    case class NoRulesApply(s:String) extends  EvalException

    abstract class Term {
      /**
        Term substitution
        */
      def substitute(variable:Int,value:Term) : Term = term_substitute(variable,value,this)

      /**
        Perform top level that is value gets substituted for variable 0
        */
      def substitute(value:Term):Term  = {
        /**
          Shift vars in value make 0 free then substitute it into body
          */
        val substituted_body = this.substitute(0,value.rshift(1))
        // Now that 0 has been substituted
        // Shift back variables in the program body
        substituted_body.lshift(1)
      }

      /**
        Term shifting with cutoff
        */
      def rshift(d:Int,c:Int) = term_shift(d,c,this)
      /**
        Term shifting
        */
      def rshift(d:Int):Term = term_shift(d,0,this)

      def lshift(d:Int):Term = this.rshift(-1)


      def map_vars(onvar:(Int,Int,Int) => Term, c:Int, term:Term) = {
        /**
          Walk over AST.        
          */
        def walk(cutoff:Int, term:Term):Term  = term match {

          case Var(x:Int,n:Int) => {
            onvar(cutoff,x,n)
          }
          case Abs(name:String,body:Term) => {
            // Entering abstraction
            Abs(name,walk(cutoff+1, body))
          }
          case App(t1:Term,t2:Term) =>
            App(walk(cutoff,t1),
              walk(cutoff,t2))
          case t1:Term => t1
          case _ => throw NoRulesApply("map_vars :Failing in mapping")
        }
        walk(c, term)
      }


      /**
        Walk through the program AST.
        Increment variable indices by d 
        if they lie above the cutoff c

        d - variable index increment
        c - variable increment cutoff 
        t - program ast
        */
      def term_shift(d:Int,c:Int,t:Term) = {

        def on_vars(cutoff:Int,x:Int,n:Int):Term = {
          if(x >= cutoff){
            Var(x+d,n+d);
          } else {
            Var(x,n+d);
          }
        }
        map_vars(on_vars,c,t);
      }


      def term_substitute(j:Int,s:Term,t:Term) : Term = {
        def on_vars(cutoff:Int , x:Int,n:Int):Term = {
          if(x == j+cutoff){
            s.rshift(cutoff)
          } else {
            Var(x,n)
          }
        }
        map_vars(on_vars,0,t)
      }

      def term_substitute_top(s:Term,body:Term):Term = {
        /**
          Shift vars in s make 0 free then substitute it into body
          */
        val substituted_body = body.substitute(0,s.rshift(1))
        // Now that 0 has been substituted
        // Shift back variables in the program body
        substituted_body.lshift(1)
      }
    }

    case class Unit extends Term
    case class True extends Term
    case class False extends Term
    case class Zero extends Term
    case class Succ(t:Term) extends Term
    case class Pred(t:Term) extends Term
    case class If(t1:Term,t2:Term,t3:Term) extends Term
    case class IsZero(t:Term) extends Term
    // n - keep track of the context length
    // id - id of the variable used for substitution
    case class Var(id:Int,n:Int) extends Term
    case class UnresolveVar(x:String) extends Term
    case class Abs(name:String,body:Term) extends Term
    case class App(t1:Term, t2:Term) extends Term


    def is_value(t:Term) : Boolean = {
      t match {
        case Abs(_,_) => true
        case t if (is_numerical(t)|| is_boolean(t)) => true
        case _ => false
      }
    }

    def is_boolean(t:Term) : Boolean = {
      t match{
        case True()| False() => true
        case _ => false
      }
    }

    def is_numerical(t:Term): Boolean = {
      t match {
        case Zero() => true
        case Succ(t) => is_numerical(t)
        case Pred(t) => is_numerical(t)
        case _ => false
      }
    }

    class LCParser extends RegexParsers {

      def value:Parser[Term] = (
        "0".r^^{_=>  Zero() }      |
          "true".r^^{_=>  True()}   |
          "false".r^^{_=> False()}
      )

      def atomic:Parser[Term] = {
        "[a-zA-Z0-9]+".r ^^{
          s => s match{
            case _ =>{
              println("atom parser found :"+s)
              UnresolveVar(s)
            }
          }
        }
      }

      def lambda:Parser[Term] = {
        "lambda ".r ~> "[a-zA-Z0-9]+".r~ ".".r~term ^^ {s=>
          println("s:"+s)
          s match {
            case (v~d~body) => {
              println("Abstraction :"+v)
              Abs(v,body);
            }
          }
        }
      }

      def expr:Parser[List[Term]] = repsep(term_top,";") | repsep(term_top,"\n")

      //      override def skipWhitespace = false
      def term_app:Parser[Term] = (term ~" ".r~atomic ^^{s =>
        s match {
          case (t~s~a) => App(t,a)
        }}
        | atomic)


      def term_top:Parser[Term] = term^^{
        t:Term =>
        def walk(p:Term,ctx:List[String]):Term ={
          p match {
            case UnresolveVar(x) => {
              val index = ctx.indexOf(x)
              Var(index,ctx.length)
            }
            case App(t1:Term,t2:Term) => App(walk(t1,ctx),walk(t2,ctx))
            case Abs(name:String,body:Term) => Abs(name,walk(body,name::ctx))
            case t => t
          }
        }
        walk(t,List[String]())
      }


      def term:Parser[Term] = (
        atomic~" ".r~term^^ {s =>
          s match {
            case (a1~k~a2) => App(a1,a2)
          }} |
          "("~> term <~")"
          | lambda
          | value
          | atomic
          | "if".r~term~"then".r~term~"else".r~term ^^ {
            s => s match {
              case("if"~t1~"then"~t2~"else"~t3) => If(t1,t2,t3)
            }}
          | """iszero""".r~term  ^^ {
            case("iszero"~v) => IsZero(v)
          }
          |"succ".r~term ^^ {
            case("succ"~v) =>
              Succ(v)
          }
          |"pred".r~term ^^ {
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

      def fromString(s:String):List[Term] = {
        parseAll(expr,s) match  {
          case Success(result,_) => result
          case f: NoSuccess => scala.sys.error(f.msg)
        }
      }
    }

      def eval1(term: Term): Term =  {
        def eval_numerical(t1:Term) = {
          val result = eval1(t1)
          require(is_numerical(result))
          result
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
          //Lambda Calculus
          case App(Abs(name:String,body:Term),v2) if is_value(v2) => {
            //println("Substituting value in abstraction "+x);
            body.substitute(v2)
          }
          case App(v1:Term,t2:Term) if is_value(v1) =>{
            App(v1,eval1(t2))
          }
          case App(t1:Term,t2:Term) => {
            val r1 = eval1(t2)
            App(r1,t2)
          }

          case _ => throw NoRulesApply("Out of rules")
        }
      }

      def eval(term:Term):Term = {
        try {
          val t = eval1(term)
          eval(t)
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

      def runFile(in_file:String): scala.Unit ={
        val reader  = new FileReader(in_file);

        val results =run(reader);
        results.map(pr => {
          val rs = eval(pr)
          print_result(rs);
        })
      }

      def print_results(terms:List[Term]):scala.Unit = terms.map(print_result)

      def num_term(prog:Term):Int =  {
        def nt(acc:Int, n:Term):Int =
          n match {
            case Zero() => acc
            case Succ(t1 :Term) => nt(acc+1,t1)
            case Pred(t1:Term)  => nt(acc-1,t1)
            case _ => throw NoRulesApply("Not a number")
          }
        nt(0,prog)
      }

      def print_result(prog:Term):scala.Unit = {
        prog match {
          case False() => println("false")
          case True() => println("true")
          case t:Term if is_numerical(t) => println(num_term(t))
          case _ => throw NoRulesApply("Out of rules")
        }
      }

      def main(args:Array[String]):scala.Unit= {
        if(args.length < 1) {
          println("Usage: stl <input-file>")
          return;
        }
        runFile(args(0))
      }
    

  }
}



