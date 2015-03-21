package edu.berkeley.cs.boom.bloomscala.codegen.c4

import edu.berkeley.cs.boom.bloomscala.codegen.CodeGenerator
import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.analysis.{Stratum, DepAnalyzer, Stratifier}
import edu.berkeley.cs.boom.bloomscala.typing.FieldType

/**
 * Base class for code generators targeting push-based dataflow systems.
 *
 * Translates programs into a generic dataflow intermediate language and provides
 * hooks that subclasses can use to instantiate that dataflow on a particular
 * runtime.
 */
object C4CodeGenerator extends CodeGenerator {

  // TODO: implement checks for unconnected ports in the generated dataflow?
  // Some ports of Table may be unconnected if the program doesn't contain deletions,
  // for example, but other operators, like HashJoin, must have all of their inputs
  // connected.

  /**
   * Translate a dataflow graph to platform-specific code.
   */

  def genExpr(expr: Node, parameterNames: List[String]): Doc = {
    expr match {
      case cr: CollectionRef =>
        //System.out.println(s"for $cr, write ${cr.lambdaArgNumber}")
        parameterNames(cr.lambdaArgNumber).toUpperCase
      case BoundFieldRef(cr, field, _) =>
        //System.out.println(s"for2 $cr, write ${cr.lambdaArgNumber} IE ${parameterNames(cr.lambdaArgNumber)}")
        //System.out.println(s"so I print ")
        //System.out.println(s"for2 field $field ${cr.collection.name}, write ${cr.lambdaArgNumber} for $field given $parameterNames")
        parameterNames(cr.lambdaArgNumber).toUpperCase <> text(cr.collection.indexOfField(field).toString)
      case PlusStatement(a, b, _) =>
        //println(s"UHOH plus $a + $b")
        genExpr(a, parameterNames) <+> plus <+> genExpr(b, parameterNames)
      case RowExpr(colExprs) =>
        ssep(colExprs.map(genExpr(_, parameterNames)), ", ")
//      case EqualityPredicate(a, b) =>
//          System.out.println(s"for some reason, I have ${genExpr(b, parameterNames).toString()} IE ${b}")
//          genExpr(a, parameterNames) <+> equal <+> genExpr(b, parameterNames)
      case ConstantColExpr(s, t) => text(s)
    }
  }

  def genRHS(arity: Int, name: String, alias: String): Doc = {
    name <> parens(ssep((0 to arity - 1).map(a => text(alias.toUpperCase + a.toString)), ", "))
  }

  def reMap(ref: ColExpr, tabMap: Map[String, Int]): ColExpr = {
    ref match {
      case BoundFieldRef(collection, name, field) => {
        //println(s"OH BOY $tabMap, $name")
        val l = BoundCollectionRef(name, collection.collection, tabMap(collection.collection.name))
        //BoundFieldRef(l, ref.collection.name, ref.field)
        BoundFieldRef(l, name, field)
      }
      case _ => ref
    }
  }

  final def generateCode(orig_program: Program, stratifier: Stratifier, depAnalyzer: DepAnalyzer): CharSequence = {
    //implicit val graph = new DataflowGraph(stratifier)
    //program.statements.foreach(rule => addElementsForRule(graph, depAnalyzer, rule, stratifier.ruleStratum(rule)))
    //generateCode(graph)

    //val schema = collection.mutable.Map[String, Seq[String]]()

    val program = orig_program

    val schema: Map[String, List[String]] = program.declarations.map { decl =>
      //val cols = (decl.keys ++ decl.values).map(k => k.name)
      val cols = 0.until((decl.keys ++ decl.values).length).map(i => decl.name + i.toString).toList
      //println(s"COLS(${decl.name}) is $cols")
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
      //val lhs = stm.lhs.name <> parens(ssep(schema(stm.lhs.name).map(text).toSeq.toSeq, ","))
      val suffix = stm.op match {
        case BloomOp.InstantaneousMerge => text("")
        case BloomOp.DeferredMerge => text("@next")
        case BloomOp.AsynchronousMerge => text("@async")
      }
      val op = suffix <+> text(":-")
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
          System.out.println(s"TUPVARS is $tupVars")
          val tabMap = 0.until(joins.length).map(i => (joins(i).name, i)).toMap
          //val tabMap = Map()
          // hack
          //val tupList = if (tupVars.isEmpty) {
          //  List("driver", "stratum")
          //} else {
          //  tupVars ++ List("stratum")
          //}
          val tupList = tupVars ++ List("stratum")
          val argLst = parens(ssep(rowExpr.cols.map(e => genExpr(e, tupList)), ", "))
          val joinLst = 0.until(joins.length).map { i =>
            //println(s"LOOKUP $i ($joins) (with $tupList)")
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
        //case ArgMin(cr: CollectionRef, groupingCols, chooseExpr, func)
        case _ => text("//bar")
      }

    }
    val doc = (tables ++ rules).toSeq.reduce(_ <@@> _)
    super.pretty(doc)
  }
}