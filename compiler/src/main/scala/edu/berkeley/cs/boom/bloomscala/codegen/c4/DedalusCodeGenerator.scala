package edu.berkeley.cs.boom.bloomscala.codegen.c4

import edu.berkeley.cs.boom.bloomscala.codegen.CodeGenerator
import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.analysis.{Stratum, DepAnalyzer, Stratifier}
import edu.berkeley.cs.boom.bloomscala.typing.FieldType


object DedalusCodeGenerator extends DatalogCodeGenerator {
  final def generateCode(orig_program: Program, stratifier: Stratifier, depAnalyzer: DepAnalyzer): CharSequence = {
    val program = orig_program


    val rules = genProgram(program, true)
    val doc = rules.toSeq.reduce(_ <@@> _)
    super.pretty(doc)
  }
}