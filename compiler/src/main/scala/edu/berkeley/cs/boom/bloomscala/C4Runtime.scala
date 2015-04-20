package edu.berkeley.cs.boom.bloomscala

import com.typesafe.scalalogging.slf4j.Logging
import edu.berkeley.cs.boom.bloomscala.exe.C4Wrapper
import org.kiama.util.Messaging
import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.parser.BudParser
import edu.berkeley.cs.boom.bloomscala.analysis._
import edu.berkeley.cs.boom.bloomscala.codegen.js.RxFlowCodeGenerator
import com.quantifind.sumac.{ArgMain, FieldArgs}
import java.io.File
import com.quantifind.sumac.validation.Required
import scala.io.Source
import edu.berkeley.cs.boom.bloomscala.codegen.CodeGenerator
import edu.berkeley.cs.boom.bloomscala.codegen.dataflow.GraphvizDataflowPrinter
import edu.berkeley.cs.boom.bloomscala.codegen.c4.C4CodeGenerator
import edu.berkeley.cs.boom.bloomscala.typing.Typer
import edu.berkeley.cs.boom.bloomscala.rewriting.StratRewrites._
//import edu.berkeley.cs.boom.bloomscala.Compiler

class C4RuntimeArgs extends FieldArgs {
  @Required
  var infile: File = null
  var port: Int = 0
}


object C4Runtime extends Logging with ArgMain[C4RuntimeArgs] {

  //def c4Play(program: Program, code: CharSequence, stratifier: Stratifier, args: C4RuntimeArgs): Unit = {
  def c4Play(wrap: C4Wrapper) {
    //wrap.install("stratum(0);")
    wrap.install("link(\"a\", \"b\", 3);")
    wrap.install("link(\"b\", \"c\", 2);")
    wrap.install("link(\"c\", \"d\", 2);")

    wrap.tick

    var res2 = wrap.dump("path")
    println(s"RES2 $res2")
    var res3 = wrap.dump("next_strat")
    println(s"RES2 $res3")

    var res4 = wrap.dump("hop2")
    println(s"HOP $res4")


    println("DO THE DEL")
    wrap.install("del_time(0);")
    res2 = wrap.dump("path")
    println(s"RES2 $res2")
    res3 = wrap.dump("next_strat")
    println(s"RES2 $res3")

    //fixpoint(wrap, maxstrat)
    wrap.tick

    var res1 = wrap.dump("stratum")
    println(s"STRAT ${res1}")

    wrap.install("del_time(1);")
    println(s"PATH IS ${wrap.dump("path").mkString(",")}")

    val tm = System.currentTimeMillis()
    for (i <- 1.until(1000)) {
      wrap.tick
    }

    println(s"1K ticks took ${System.currentTimeMillis() - tm}")
  }


  def main(args: C4RuntimeArgs) {
    implicit val messaging = new Messaging

    //val program = compileToIntermediateForm(Source.fromFile(args.infile).mkString)
    val program = Compiler.nameAndType(Source.fromFile(args.infile).mkString)
    val (code, stratifier) = Compiler.generateCode(program, C4CodeGenerator)


    val maxStratum = program.nodes.map { n =>
      val ret = n match {
        case s: Statement => stratifier.ruleStratum(s)
        case d: CollectionDeclaration => stratifier.collectionStratum(d)
      }
      println(s"RET $ret for $n")
      ret
    }.filter(r => r != Stratum.lastStratum).max

    println(s"running code:\n$code")

    val wrap = new C4Wrapper("foo", code.toString, maxStratum, args.port)
    wrap.start


    wrap.tick

    println(s"OK NOW")

    /*
    while (true) {
      var res = wrap.dump("ping")
      println(s"PING: $res")
      res = wrap.dump("pong")
      println(s"PONG: $res")
      Thread.sleep(1000)
    }
    */
    // now screw around
    //c4Play(program, code, stratifier, args)
    Thread.sleep(10000)

  }
}