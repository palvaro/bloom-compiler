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

class CompilerArgs extends FieldArgs {
  @Required
  var infile: File = null
  var target: String = "RxFlow"
}


object Compiler extends Logging with ArgMain[CompilerArgs] {

  def nameAndType(src: CharSequence)(implicit messaging: Messaging): Program = {
    try {
      val parseResults = BudParser.parseProgram(src)
      val named = new Namer(messaging).resolveNames(parseResults)
      val typed = new Typer(messaging).resolveTypes(named)
      typed
    } catch { case e: Exception =>
      logger.error("Compilation failed", e)
      throw e
    } finally {
      if (messaging.messagecount != 0) {
        messaging.report()
        // TODO: this is fine for now for simple tests, but in the future
        // `compile` should return more detailed information for consumption
        // by unit tests
        throw new CompilerException("Compilation had error messages")
      }
    }
  }

  /**
   * Compiles a program, but stops short of code generation.
   */
  def compileToIntermediateForm(src: CharSequence)(implicit messaging: Messaging): Program = {
    val typed = nameAndType(src)
    val depAnalyzer = new DepAnalyzer(typed)
    val stratifier = new Stratifier(depAnalyzer)
    if (!stratifier.isTemporallyStratifiable(typed)) {
      throw new StratificationError("Program is unstratifiable")
    }
    typed
  }

  def analysis(program: Program)(implicit messaging: Messaging): (Stratifier, DepAnalyzer) = {
    val depAnalyzer = new DepAnalyzer(program)
    val stratifier = new Stratifier(depAnalyzer)
    (stratifier, depAnalyzer)
  }

  def generateCode(program: Program, generator: CodeGenerator)(implicit messaging: Messaging): (CharSequence, Stratifier) = {
    generator match {
      case C4CodeGenerator =>
        val firstRewrite = staggerNonmonotonics(program)
        val (stratifier, depAnalyzer) = analysis(program)
        val stratified = addStratConditions(firstRewrite, stratifier)
        (generator.generateCode(stratified, stratifier, depAnalyzer), stratifier)
      case _ =>
        val (stratifier, depAnalyzer) = analysis(program)
        (generator.generateCode(program, stratifier, depAnalyzer), stratifier)
    }
  }

  def c4Play(program: Program, code: CharSequence, stratifier: Stratifier): Unit = {
    println(s"*** $code ***")
    val maxStratum = program.nodes.map { n =>
      val ret = n match {
        case s: Statement => stratifier.ruleStratum(s)
        case d: CollectionDeclaration => stratifier.collectionStratum(d)
      }
      println(s"RET $ret for $n")
      ret
    }.filter(r => r != Stratum.lastStratum).max

    val wrap = new C4Wrapper("foo", code.toString, maxStratum)
    wrap.start


    //wrap.install("stratum(0);")
    wrap.install("link(\"a\", \"b\", 3);")
    wrap.install("link(\"b\", \"c\", 2);")
    wrap.install("link(\"c\", \"d\", 2);")

    wrap.tick

    var res2 = wrap.dump("path")
    println(s"RES2 $res2")
    var res3 = wrap.dump("next_strat")
    println(s"RES2 $res3")

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
  }


  def main(args: CompilerArgs) {
    implicit val messaging = new Messaging
    val generator = args.target.toLowerCase match {
      case "rxflow" => RxFlowCodeGenerator
      case "dataflow" => GraphvizDataflowPrinter
      case "c4" => C4CodeGenerator
      case unknown => throw new IllegalArgumentException(s"Unknown target platform $unknown")
    }
    val program = compileToIntermediateForm(Source.fromFile(args.infile).mkString)
    val (code, stratifier) = generateCode(program, generator)


    // now screw around
    args.target.toLowerCase match {
      case "c4" =>
        c4Play(program, code, stratifier)
      case _ => println(code)

    }

  }
}