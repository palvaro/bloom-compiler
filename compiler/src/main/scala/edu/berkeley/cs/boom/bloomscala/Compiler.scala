package edu.berkeley.cs.boom.bloomscala

import com.typesafe.scalalogging.LazyLogging
import org.kiama.attribution.Attribution
import org.kiama.util.{ErrorEmitter, Emitter, Messaging}
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
import edu.berkeley.cs.boom.bloomscala.codegen.c4.{C4CodeGenerator, DedalusCodeGenerator}
import edu.berkeley.cs.boom.bloomscala.typing.Typer
import edu.berkeley.cs.boom.bloomscala.rewriting.C4Rewrites._
import edu.berkeley.cs.boom.bloomscala.rewriting.DedalusRewrites._
import edu.berkeley.cs.boom.bloomscala.rewriting.StratRewrites._

class CompilerArgs extends FieldArgs {
  @Required
  var infile: File = null
  var target: String = "RxFlow"
}

object Compiler extends LazyLogging with ArgMain[CompilerArgs] {
  def nameAndType(src: CharSequence, context: File = new File("."))(implicit emitter: Emitter): Program = {
    val typer = new Typer()
    val namer = new Namer()
    try {
      val parseResults = BudParser.parseProgram(src)
      val expanded = processRequires(parseResults, context)
      Attribution.initTree(expanded)
      val named = namer.resolveNames(expanded)
      val typed = typer.resolveTypes(named)
      // the thinking is that this rewrite should be perfectly hygienic. :)
      staggerNonmonotonics(typed)

    } catch {
      case e: Exception =>
        logger.error("Compilation failed", e)
        throw e
    } finally {
      //FIXME Sort this out
      val allError = typer.errors ++ namer.errors
      if (allError.nonEmpty) {
        Messaging.report(allError, emitter)
        // TODO: this is fine for now for simple tests, but in the future
        // `compile` should return more detailed information for consumption
        // by unit tests
        throw new CompilerException("Compilation had error messages")
      }
    }
  }

  def processRequires(program: Program, context: File): Program = {
    val nodes = program.nodes.map{
      case Require(filename) =>
        val file = new File(context, filename)
        val subProgram = BudParser.parseProgram(Source.fromFile(file).getLines().mkString("\n"))
        subProgram
      case x => x
    }
    Program(nodes)
  }

  /**
   * Compiles a program, but stops short of code generation.
   */
  def compileToIntermediateForm(src: CharSequence)(implicit emitter: Emitter): Program = {
    nameAndType(src)
  }

  def analysis(program: Program)(implicit emitter: Emitter): (Stratifier, DepAnalyzer) = {
    val depAnalyzer = new DepAnalyzer(program)
    val stratifier = new Stratifier(depAnalyzer)
    if (!stratifier.isTemporallyStratifiable(program)) {
      throw new StratificationError("Program is unstratifiable")
    }
    (stratifier, depAnalyzer)
  }

  def generateCode(program: Program, generator: CodeGenerator)(implicit emitter: Emitter): (CharSequence, Stratifier) = {
    val (stratifier, depAnalyzer) = analysis(program)
    generator match {
      case C4CodeGenerator =>
        //val stratified = staggerChannels(addStratConditions(program, stratifier))
        val stratified = addStratConditions(staggerChannels(program), stratifier)
        (generator.generateCode(stratified, stratifier, depAnalyzer), stratifier)
      case DedalusCodeGenerator =>
        val framed = frameRules(program)
        (generator.generateCode(framed, stratifier, depAnalyzer), stratifier)
      case _ => (generator.generateCode(program, stratifier, depAnalyzer), stratifier)
    }
  }

  def main(args: CompilerArgs) {
    implicit val emitter = new ErrorEmitter
    val generator = args.target.toLowerCase match {
      case "rxflow" => RxFlowCodeGenerator
      case "dataflow" => GraphvizDataflowPrinter
      case "c4" => C4CodeGenerator
      case "dedalus" => DedalusCodeGenerator
      case unknown => throw new IllegalArgumentException(s"Unknown target platform $unknown")
    }
    val program = nameAndType(Source.fromFile(args.infile).mkString, args.infile.getParentFile)
    val (code, stratifier) = generateCode(program, generator)
    println(code)
  }
}