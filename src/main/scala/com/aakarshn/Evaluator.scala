package com.aakarshn 


import scala.util.parsing.combinator._
import scala.io.Source
import java.io._
import Syntax._;
/**
  A direct port of the evaluator for simply typed lambda calculus.    
  */
object Evaluator  {

  val parser = new LambdaParserCtx()
  val repl_promt = "[STL] $ "

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

  def eval1(term:Term,ctx:Context): Term =  {

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
      case  Let(name:String,v2:Term,body:Term) if is_value(v2) => {
        body.substitute(v2)
      }
      case  Let(name:String,t2:Term,body:Term) => {
        Let(name,eval1(t2,ctx),body)
      }
        //
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


  def eval_empty(term:Term):Term = {
    eval(term,emptycontext)
  }

  def eval(term:Term,ctx:Context):Term = {
    try {
      val t = eval1(term,ctx)
      eval(t,ctx)
    } catch{
      case ex:NoRulesApply => term
    }
  }

  def processString(s :String , ctx: Context) = {
    val cmds:List[CtxCmd] = parser.parseReader(s);
    processCommandList(cmds,ctx)
  }

  def processFile(in_file:String) :scala.Unit = {
    processFile(in_file,emptycontext)
  }

  def processFile(in_file:String,ctx:Context):scala.Unit ={
    val reader  = new FileReader(in_file);
    val lst:List[CtxCmd] = parser.parseReader(reader);
    processCommandList(lst,ctx)
  }

  def processCommandList(lst:List[CtxCmd],ctx:Context) :scala.Unit = { 
    def r(cmd:CtxCmd,ctx:Context,acc:List[Command]) ={
      val (rcmd,rctx)= cmd(ctx)
      (rcmd::acc,rctx)
    }

    var rctx =  ctx
    var rcmds = List[Command]();

    for(cmd <-lst) {
      val k = cmd(rctx)
      val c = k._1
      rctx = k._2
      rctx  = processCommand(c,rctx)
    }
  }

  def processCommand(cmd:Command,ctx:Context):Context =  {
    cmd match {
      case Eval(t)  => {
        val t1 = eval(t,ctx)
        print_result(t1)
        println()
        return ctx
      }
      case Bind(x,b) => {
        val binding = evalBinding(b,ctx)
        println("Adding x:"+binding+"to ctx" +ctx)
        return addBinding(ctx,x,binding)
      }
    }
  }

  def evalBinding(b:Binding,ctx:Context) ={
    b match {
      case TmAbbBind(t) =>
        val t1 = eval(t,ctx)
        TmAbbBind(t1)
      case t => t
    }
  }

  def parse(s:String,ctx:Context):List[Term] =  {

    val lst = parser.fromString(s,ctx)

    def r(cmd:CtxTerm,ctx:Context,acc:List[Term]) ={
      val (rcmd,rctx)= cmd(ctx)
      (rcmd::acc,rctx)
    }

    var rctx:Context = ctx;
    var rtms:List[Term] = List[Term]();

    for(c <- lst) {
      val k = r(c,rctx,rtms);
      rtms = k._1
      rctx = k._2
    }
    rtms
  }

  def parse1(s:String,ctx:Context):Term =  parse(s,ctx)(0)

  def run_empty(prog: String) = {
    run(prog,emptycontext)
  }

  def run(prog: String,ctx:Context) = {
    val t = parse(prog,ctx)
    t.map(eval(_,ctx))
  }

  def run1(prog: String,ctx:Context) = {
    run(prog,ctx)(0)
  }

  def repl():scala.Unit = {
    var ok = true
    while(ok) {
      print(repl_promt)
      val line = readLine();
      try{
        line match {
          case ":q" =>  ok = false;
          case ":l" =>  {
            val fname  = line.split(" ")(1)
            Evaluator.processFile(fname,emptycontext)
          }
          case _  => processString(line,emptycontext)
        }
      }
      catch {
        case ex:Exception  => ex.printStackTrace()
        case ex:NoRulesApply  => ex.printStackTrace()
      }
    }
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


