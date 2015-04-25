package edu.berkeley.cs.boom.bloomscala.codegen.c4

import edu.berkeley.cs.boom.bloomscala.codegen.CodeGenerator
import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.analysis.{Stratum, DepAnalyzer, Stratifier}
import edu.berkeley.cs.boom.bloomscala.typing.FieldType


trait DatalogCodeGenerator extends CodeGenerator {

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
      case ConstantColExpr(s, FieldType("string")) => dquotes(s)
      case ConstantColExpr(s, t) => text(s)
      //case NestedTupleRef(BoundCollectionRef(_, CollectionDeclaration(_, _, keys, values), _), _) =>
      case NestedTupleRef(bcr, typ) =>
        //println(s"gen up a string from ${keys ++ values} and $parameterNames")
        val cols = (bcr.collection.keys ++ bcr.collection.values).map(c => BoundFieldRef(bcr, c.name, c))

        //text("FOO")
        genExpr(RowExpr(cols), parameterNames)
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

  def genProgram(program: Program, suffix: Boolean): Traversable[Doc] = {
    val schema: Map[String, List[String]] = program.declarations.map { decl =>
      val cols = 0.until((decl.keys ++ decl.values).length).map(i => decl.name + i.toString).toList
      (decl.name, cols)
    }.toMap

    val rules = program.statements.map { stm =>
      //val name = stm.lhs.name
      // c4 and dedalus targets use handle deletion differently, but share the del_TAB convention
      val name = if (stm.op == BloomOp.Delete)  {
        "del_" + stm.lhs.name
      } else {
        stm.lhs.name
      }

      var lhsArgs = parens(ssep(schema(stm.lhs.name).map(i => i.toUpperCase).map(text), ", "))
      val op: Doc = if (suffix) {
        val suff: Doc = stm.op match {
          case BloomOp.InstantaneousMerge => text("")
          case BloomOp.DeferredMerge => text("@next")
          case BloomOp.AsynchronousMerge => text("@async")
          case BloomOp.Delete => text("")
          case f => text(s"// unhandled: $f")
        }
        suff <+> ":-"
      } else {
        text(" :-")
      }

      stm.rhs match {
        case cr: CollectionRef =>
          name <> lhsArgs <> op <+> cr.name <> lhsArgs <> semi
        case MappedCollection(cr: CollectionRef, tupVars, rowExpr: RowExpr) =>
          val tupList = tupVars ++ List("stratum")
          val arglst = parens(ssep(rowExpr.cols.map(e => genExpr(e, tupList)), ", "))
          name <> arglst <> op <+> genRHS(schema(cr.name).length, cr.name, tupList(0)) <> semi
        case JoinedCollections(joins, preds, tupVars, rowExpr: RowExpr) =>
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
          name <> argLst <> op <+> ssep(joinLst ++ qualLst, ", ") <> semi
        case NotIn(left: CollectionRef, anti: CollectionRef) =>
          name <> lhsArgs <> op <+> left.name <> lhsArgs <> "," <+> "notin" <+> anti.name <> lhsArgs <> semi
        case Facts(rows) => {
          /*
          if (suffix) {
            rows.map
          } else {
          */
          println(s"FACTS!!!")
          rows.map(r => name <> parens(genExpr(r, List())) <> semi).reduce(_ <@@> _)
          //}
        }
        case _ => text(s"// UNRECOGNIZED: ${stm.rhs}")
      }
    }
    rules
  }
}