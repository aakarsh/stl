package com.aakarshn 

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._
import Syntax._

/**
  A direct port of the evaluator for simply typed lambda calculus.    
  */
object Evaluator {

  val parser = new LambdaParser()
  val repl_promt = "[STL] $ "
  val debug = true;

  /*
  def is_value(t:Term) : Boolean = {
    t match {
      case Abs(_,_,_) => true
       case t if (is_numerical(t) || is_boolean(t)) => true
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

   */
  def evalTerm1(term:Term,ctx:Context): Term =  {

    def eval_numerical(t1:Term,ctx:Context) = {
      val result = evalTerm1(t1,ctx)
      require(result.is_numerical)
      result
    }

    term match {
      case  If(True(),t2,t3) => t2
      case  If(False(),t2,t3) => t3
      case  If(t1:Term,t2:Term,t3:Term)   => If(evalTerm1(t1,ctx),t2,t3)
      case  Succ(Pred(t1)) => t1
      case  Succ(t1)  =>  Succ(eval_numerical(t1,ctx))
      case  Pred(Succ(t1)) => t1
      case  Pred(t1)  => Pred(eval_numerical(t1,ctx))
      case  IsZero(Zero()) => True()
      case  IsZero(Succ(t:Term)) => False()
      case  IsZero(Pred(t:Term)) => False()
      case  IsZero(t1) => IsZero(evalTerm1(t1,ctx))
      case  Let(name:String,v2:Term,body:Term) if v2.is_value => {
        body.substitute(v2)
      }
      case  Let(name:String,t2:Term,body:Term) => {
        Let(name,evalTerm1(t2,ctx),body)
      }
      //Lambda Calculus
      case App(Abs(name:String,_,body:Term),v2) if v2.is_value => {
        body.substitute(v2)
      }
      case App(v1:Term,t2:Term) if v1.is_value =>{
        App(v1,evalTerm1(t2,ctx))
      }
      case App(t1:Term,t2:Term) => {
        val r1 = evalTerm1(t2,ctx)
        App(r1,t2)
      }
      case fixed_term@Fix(v1:Term) if v1.is_value =>{
        v1 match {
          case Abs(_,_,abs_body) => fixed_term.substitute(abs_body)
          case _ => 
            throw NoRulesApply("Fix passed in non abstraction term "+v1)
        }
      }
      case Fix(t1:Term) => 
        Fix(evalTerm1(t1,ctx))
      case _ => throw NoRulesApply("Out of rules")
    }
  }

  def eval_empty(term:Term):Term = {
    evalTerm(term,emptycontext)
  }

  def evalTerm(term:Term,ctx:Context):Term = {
    try {
      val t = evalTerm1(term,ctx)
      evalTerm(t,ctx)
    } catch{
      case ex:NoRulesApply => term
    }
  }

  def typeof(term:Term,ctx:Context):Type = {
    term match {
      case True()    => TyBool()
      case False()   => TyBool()
      case Unit()    => TyUnit()
      case Zero()    => TyNat()
      case StringTerm(t) => TyString()
      case NumberTerm(v:Double) => TyFloat()

      case App(t1:Term,t2:Term) =>{
        val tyT1 = typeof(t1,ctx)
        val tyT2 = typeof(t2,ctx)
        tyT1 match {
          case TyArrow(ty11,ty12) =>
            // if argument type equal type of argument 
            // return result typen
            if (ty11 == tyT2) 
              ty12
            else {
              println("Application : "+ty11+"argument type!= "+tyT2+"argumeent");
              return TyAny()
            }
          case _  => {
              println("Aplying non method "+tyT1);
              return TyAny()
          }
        }
      }
      case Abs(x,tyT1,body:Term)=>
        val rctx = addBinding(ctx,x,VarBinding(tyT1))
        val bodyType = typeof(body,rctx)
        TyArrow(tyT1,bodyType)
      case If(t1:Term,t2:Term,t3:Term) =>
        val ty1 = typeof(t1,ctx)
        if(ty1 != TyBool()){
          println("Warning: If conditional non boolean");
          TyAny()
        }
        val ty2 = typeof(t2,ctx)
        val ty3 = typeof(t3,ctx)
        if (ty2 != ty3){
          println("Warning: Arms of conditional are not the same ty2:"+ty2+" ty3:"+ty3);
          TyAny()
        }else {
          ty2;
        }
      case Succ(t1)  => 
        val ty = typeof(t1,ctx)
        if(ty != TyNat()){
          println("Warning :Type in Succ wrong "+ty)
          TyAny()
        }else{
          TyNat()
        }
      case Pred(t1)  => 
        val ty = typeof(t1,ctx)
        if(ty != TyNat()){
          println("Warning: Type in Succ wrong "+ty)
          TyAny()
        }else{
          TyNat()
        }      
      case IsZero(t1)  => 
        val ty = typeof(t1,ctx)
        if(ty != TyNat()){
          println("Warning:Type in Succ wrong "+ty)
          TyAny()
        }else {
          TyNat()
        }
      case _ => 
        println("Warning: Unknown type for " + term)
        TyAny()
    }
  }

  def processString(s:String):scala.Unit = 
    processString(s,emptycontext)

  def processString(s :String , ctx: Context) = {
    val cmds:List[CtxCmd] = parser.parseCommands(s);
    processCommandList(cmds,ctx)
  }

  def processFile(in_file:String):Context =   processFile(in_file,emptycontext)

  def processFile(in_file:String,ctx:Context):Context ={
    val reader  = new FileReader(in_file);
    val lst:List[CtxCmd] = parser.parseCommands(reader);
    processCommandList(lst,ctx)
  }


  def processCommandList(lst:List[CtxCmd],ctx:Context) :Context = { 
    var rctx =  ctx

    for(cmd <-lst) {
      val k = cmd(rctx)
      val c = k._1
      rctx = k._2
      rctx  = processCommand(c,rctx)
    }
    rctx
  }

  /**
    Returns context modified as a resutl of evaluating command
  */
  def processCommand(cmd:Command,ctx:Context):Context =  {
    cmd match {
      case Eval(t)  => {
        val ty = typeof(t,ctx)
        val t1 = evalTerm(t,ctx)
        print_result(t1)
        print(":") 
        print_result(ty)
        println()
        return ctx
      }
      case Bind(x,b) => {
        val binding = evalBinding(b,ctx)
        if (debug) println("Adding x:"+binding+"to ctx :[" +ctx+"]")
        return addBinding(ctx,x,binding)
      }
    }
  }


  def processCommandInternal(cmd:Command,ctx:Context,th:TermHandler):Context =  {
    cmd match {
      case Eval(t)  => {
        val t1 = evalTerm(t,ctx)
        th(t1)
//        print_result(t1)
//        println()
        return ctx
      }
      case Bind(x,b) => {
        val binding = evalBinding(b,ctx)
        if (debug) println("Adding x:"+binding+"to ctx" +ctx)
        return addBinding(ctx,x,binding)
      }
    }
  }

  def evalBinding(b:Binding,ctx:Context) ={
    b match {
      case TmAbbBind(t) =>
        val t1 = evalTerm(t,ctx)
        TmAbbBind(t1)
      case t => t
    }
  }

  def repl():scala.Unit = {
    var ok = true
    var ctx = emptycontext;
    while(ok) {
      print(repl_promt)
      val line = readLine();
      try{
        line match {
          case ":q" =>  ok = false;
          case ":l" =>  {
            val fname  = line.split(" ")(1)
            ctx = Evaluator.processFile(fname,ctx)
          }
          case _  => 
            ctx = processString(line,ctx)
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

  def print_result(ty:Type):scala.Unit ={
    ty match {
      case  TyBool()=> print("Bool")
      case  TyNat()=> print("Nat")
      case  TyString()=> print("String")
      case  TyUnit()=> print("Unit")
      case  TyInert(ty)=> print_result(ty)
      case  TyAny()=> print("Any")
      case  TyArrow(ty1:Type,ty2:Type)=>
        print_result(ty1)
        print("->")
        print_result(ty2)
    }
  }

  def print_result(prog:Term):scala.Unit = {
    prog match {
      case False() => print("false")
      case True() => print("true")
      case App(t1:Term,t2:Term) => print_result(t1) ; print(" "); print_result(t2);
      case Abs(name:String,_,body:Term) => print("lambda "+name+". "); print_result(body);
      case Var(x:Int,_) => print(x)
      case t:Term if t.is_numerical => print(num_term(t))
      //for identifiers
      case Succ(t) => print("succ "); print_result(t)
      case Pred(t) => print("pred "); print_result(t)
      case StringTerm(t) => 
        print("\""+t+"\"");
      case x => 
        println("Unkown term "+x);
        throw NoRulesApply("print_result:Out of rules")
    }
  }


}


