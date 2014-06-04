package com.aakarshn 


import scala.util.parsing.combinator._
import scala.io.Source
import java.io._
import Syntax._;
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


  def eval1(term: Term,ctx : Context ): Term =  {

    def eval_numerical(t1:Term,ctx:Context) = {
      val result = eval1(t1,ctx)
      require(is_numerical(result))
      result
    }

    term match {
      case  If(True(),t2,t3) => t2
      case  If(False(),t2,t3) => t3
      case  If(t1:Term,t2:Term,t3:Term)   => If(eval1(t1,ctx),t2,t3)
      case  Succ(Pred(t1)) => t1
      case  Succ(t1)  =>  Succ(eval_numerical(t1,ctx))
      case  Pred(Succ(t1)) => t1
      case  Pred(t1)  => Pred(eval_numerical(t1,ctx))
      case  IsZero(Zero()) => True()
      case  IsZero(Succ(t:Term)) => False()
      case  IsZero(Pred(t:Term)) => False()
      case  IsZero(t1) => IsZero(eval1(t1,ctx))
      //Lambda Calculus
      case App(Abs(name:String,body:Term),v2) if is_value(v2) => {
        //println("Substituting value in abstraction "+x);
        body.substitute(v2)
      }

      case App(v1:Term,t2:Term) if is_value(v1) =>{
        App(v1,eval1(t2,ctx))
      }

      case App(t1:Term,t2:Term) => {
        val r1 = eval1(t2,ctx)
        App(r1,t2)
      }
      case _ => throw NoRulesApply("Out of rules")
    }
  }

  // def eval(term:Term):Term = {
  //   eval(term,emptycontext)
  // }

  def eval(term:Term,ctx:Context):Term = {
    try {
      val t = eval1(term,ctx)
      eval(t,ctx)
    } catch{
      case ex:NoRulesApply => term
    }
  }


  def parse(s:String,ctx:Context):List[Term] =  {
    val lst = new LambdaParserCtx().fromString(s,ctx)

    def r(cmd:CtxTerm,ctx:Context,acc:List[Term]) ={
      val (rcmd,rctx)= cmd(ctx)
      (rcmd::acc,rctx)
    }

    var rctx = ctx;
    var rtms = List[Term]();

    for(c <- lst) {
      val k = r(c,rctx,rtms);
      rtms = k._1
      rctx = k._2
    }
    rtms
  }

  def parse1(s:String,ctx:Context):Term =  parse(s,ctx)(0)

  def run(prog: String,ctx:Context) = {
    val t = parse(prog,ctx)
    t.map(eval(_,ctx))
  }

  /*
  def run(reader:Reader,ctx:Context) = {
    //TODO need to return a nwe context
    val parsed_result = new LambdaParserCtx().fromReader(reader,ctx)
    parsed_result.map(t:Term => eval(t,ctx))
  }
   */

  def run1(prog: String,ctx:Context) = {
    run(prog,ctx)(0)
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
        val res = run(line,emptycontext)
        println(res)
        print_results(run(line,emptycontext))
      }
    }
  }

  /*
  def runFile(in_file:String): scala.Unit ={
    runFile(in_file,emptycontext)
  }

  def runFile(in_file:String,ctx:Context): scala.Unit ={
    val reader  = new FileReader(in_file);

    val results =run(reader,ctx);
    results.map(pr => {
      val rs = eval(pr,ctx)
      print_result(rs);
      println("");
    })
  }
   */

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


