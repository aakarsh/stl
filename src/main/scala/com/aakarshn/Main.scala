package com.aakarshn 

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

import com.aakarshn._

/**
  A direct port of the evaluator for simply typed lambda calculus.
  */
object Main {

  def main(args:Array[String]): scala.Unit= {
    if(args(0) == "-i"){
      Evaluator.repl()
      return
    }
    
    if(args(0) == "-s"){
      if(args.length < 2)
      Evaluator.processString(args(1))
      return;
    }
    if(args.length < 1) {
      println("Usage: stl <input-file>")
      return;
    }
    Evaluator.processFile(args(0))
  }
}
