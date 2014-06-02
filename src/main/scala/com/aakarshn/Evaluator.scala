import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

package com.aakarshn {

  /**
    A direct port of the evaluator for simply typed lambda calculus.    
    */
  object Evaluator  {

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
        case NumberTerm(_) => true
        case Succ(t) => is_numerical(t)
        case Pred(t) => is_numerical(t)
        case _ => false
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

    def parse(s:String):List[Term] =  new LambdaParser().fromString(s)

    def parse1(s:String):Term =  new LambdaParser().fromString(s)(0)

    def run(prog: String) = {
      val t = parse(prog)
      t.map(eval _)
    }

    def run(reader:Reader) = {
      val parsed_result = new LambdaParser().fromReader(reader)
      parsed_result.map(eval _);
    }

    def run1(prog: String) = {
      run(prog)(0)
    }

    def repl():scala.Unit = {
      var ok = true;
      while(ok) {
        print("stl>")

        val line = readLine();
        if(line == ":q"){
          ok = false
        }
        if(ok){
          val res = run(line)
          println(res)
          print_results(run(line))
        }
      }
    }

    def runFile(in_file:String): scala.Unit ={
      val reader  = new FileReader(in_file);

      val results =run(reader);
      results.map(pr => {
        val rs = eval(pr)
        print_result(rs);
        println("");
      })
    }

    def print_results(terms:List[Term]):scala.Unit = terms.map({ term =>
      print_result(term) ;
      println("");
    })

    def num_term(prog:Term):Int =  {
      def nt(acc:Int, n:Term):Int =
        n match {
          case Zero() => acc
          case NumberTerm(n:Double) => n.toInt
          case Succ(t1 :Term) => nt(acc+1,t1)
          case Pred(t1:Term)  => nt(acc-1,t1)
          case _ => throw NoRulesApply("num_term:Not a number")
        }
      nt(0,prog)
    }

    def print_result(prog:Term):scala.Unit = {
      prog match {
        case False() => print("false")
        case True() => print("true")
        case App(t1:Term,t2:Term) => print_result(t1) ; print(" "); print_result(t2);
        case Abs(name:String,body:Term) => print("lambda "+name+". "); print_result(body);
        case Var(x:Int,_) => print(x)
        case t:Term if is_numerical(t) => print(num_term(t))
        case _ => throw NoRulesApply("print_result:Out of rules")
      }
    }
  }

}
