package edu.berkeley.cs.boom.bloomscala.codegen.c4

import edu.berkeley.cs.boom.bloomscala.codegen.CodeGenerator
import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.analysis.{Stratum, DepAnalyzer, Stratifier}
import edu.berkeley.cs.boom.bloomscala.typing.FieldType


object C4CodeGenerator extends CodeGenerator {

  def genExpr(expr: Node, parameterNames: List[String]): Doc = {
    expr match {
      case cr: CollectionRef =>
        parameterNames(cr.lambdaArgNumber).toUpperCase
      case BoundFieldRef(cr, field, _) =>
        parameterNames(cr.lambdaArgNumber).toUpperCase <> text(cr.collection.indexOfField(field).toString)
      case PlusStatement(a, b, _) =>
        genExpr(a, parameterNames) <+> plus <+> genExpr(b, parameterNames)
      case RowExpr(colExprs) =>
        ssep(colExprs.map(genExpr(_, parameterNames)), ", ")
      case ConstantColExpr(s, t) => text(s)
    }
  }

  def genRHS(arity: Int, name: String, alias: String): Doc = {
    name <> parens(ssep((0 to arity - 1).map(a => text(alias.toUpperCase + a.toString)), ", "))
  }

  def reMap(ref: ColExpr, tabMap: Map[String, Int]): ColExpr = {
    ref match {
      case BoundFieldRef(collection, name, field) => {
        val l = BoundCollectionRef(name, collection.collection, tabMap(collection.collection.name))
        BoundFieldRef(l, name, field)
      }
      case _ => ref
    }
  }

  final def generateCode(orig_program: Program, stratifier: Stratifier, depAnalyzer: DepAnalyzer): CharSequence = {
    val program = orig_program
    val schema: Map[String, List[String]] = program.declarations.map { decl =>
      val cols = 0.until((decl.keys ++ decl.values).length).map(i => decl.name + i.toString).toList
      (decl.name, cols)
    }.toMap

    val tables = program.declarations.map { decl =>
      val cols = decl.keys ++ decl.values
      val typs = cols.map { m =>
        m.typ match {
          case FieldType(s) => text(s)
        }
      }
      "define" <> parens(decl.name <> comma <+> braces(ssep(typs, ", "))) <> semi
    }

    val rules = program.statements.map { stm =>

/*
      val suffix = stm.op match {
        case BloomOp.InstantaneousMerge => text("")
        case BloomOp.DeferredMerge => text("@next")
        case BloomOp.AsynchronousMerge => text("@async")
      }
*/
      //val op = suffix <+> text(":-")
      val op = text(":-")
      val name = stm.lhs.name
      var lhsArgs = parens(ssep(schema(name).map(i => i.toUpperCase).map(text), ", "))

      stm.rhs match {
        case cr: CollectionRef =>
          name <> lhsArgs <+> op <+> cr.name <> lhsArgs <> semi
        case MappedCollection(cr: CollectionRef, tupVars, rowExpr) =>
          val tupList = tupVars ++ List("stratum")
          val arglst = parens(ssep(rowExpr.cols.map(e => genExpr(e, tupList)), ", "))
          name <> arglst <+> op <+> genRHS(schema(cr.name).length, cr.name, tupList(0)) <> semi
        case JoinedCollections(joins, preds, tupVars, rowExpr) =>
          val tabMap = 0.until(joins.length).map(i => (joins(i).name, i)).toMap
          // this is a hack; the effects of the rewrite should be managed by the rewrite
          val tupList = tupVars ++ List("stratum")
          val argLst = parens(ssep(rowExpr.cols.map(e => genExpr(e, tupList)), ", "))
          val joinLst = 0.until(joins.length).map { i =>
            genRHS(schema(joins(i).name).length, joins(i).name, tupList(i))
          }
          val qualLst = preds.map(p => p match {
            case EqualityPredicate(a, b) => {
              genExpr(reMap(a, tabMap), tupList) <+> "==" <+> genExpr(reMap(b, tabMap), tupList)
            }
          })
          name <> argLst <+> op <+> ssep(joinLst ++ qualLst, ", ") <> semi
        case NotIn(left: CollectionRef, anti: CollectionRef) =>
          name <> lhsArgs <+> op <+> left.name <> lhsArgs <> "," <+> "notin" <+> anti.name <> lhsArgs <> semi
        case _ => text(s"// UNRECOGNIZED: ${stm.rhs}")
      }
   }
    val doc = (tables ++ rules).toSeq.reduce(_ <@@> _)
    super.pretty(doc)
  }
}