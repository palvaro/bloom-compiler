package edu.berkeley.cs.boom.bloomscala.codegen.c4

import edu.berkeley.cs.boom.bloomscala.codegen.CodeGenerator
import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.analysis.{Stratum, DepAnalyzer, Stratifier}
import edu.berkeley.cs.boom.bloomscala.typing.FieldType
import edu.berkeley.cs.boom.bloomscala.typing.CollectionType



object C4CodeGenerator extends DatalogCodeGenerator {
  final def generateCode(orig_program: Program, stratifier: Stratifier, depAnalyzer: DepAnalyzer): CharSequence = {
    val program = orig_program

    val tables = program.declarations.map { decl =>
      val cols = decl.keys ++ decl.values
      val typs:List[Doc] = cols.map {
        case Field(n, FieldType.BloomLocation) => text("@string")
        case Field(_, FieldType(t)) => text(t)
      }
      val tab = "define" <> parens("del_" <> decl.name <> comma <+> braces(ssep(typs, ", "))) <> semi
      if (decl.collectionType == CollectionType.Table) {
        "define" <> parens(decl.name <> comma <+> braces(ssep(typs, ", "))) <> semi <@> tab
      } else {
        tab
      }

    }

    val rules = genProgram(program, false)
    val doc = (tables ++ rules).toSeq.reduce(_ <@@> _)

    super.pretty(doc)
  }
}