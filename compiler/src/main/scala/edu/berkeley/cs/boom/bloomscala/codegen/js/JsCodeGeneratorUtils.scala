package edu.berkeley.cs.boom.bloomscala.codegen.js

import edu.berkeley.cs.boom.bloomscala.ast._
import org.kiama.output.PrettyPrinter
import edu.berkeley.cs.boom.bloomscala.ast.BoundFieldRef
import edu.berkeley.cs.boom.bloomscala.ast.PlusStatement
import scala.collection.immutable
import edu.berkeley.cs.boom.bloomscala.parser.BloomPrettyPrinter

/**
 * Mixin that provides utility functions for Javascript code generation.
 *
 * Methods here aren't tailored for particular Javascript libraries, so
 * they should be able to be re-used by alternative JS code generators.
 */
trait JsCodeGeneratorUtils extends PrettyPrinter {

  def methodCall(target: Doc, methodName: Doc, args: Doc*): Doc = {
    target <> dot <> functionCall(methodName, args: _*)
  }

  def functionCall(functionName: Doc, args: Doc*): Doc = {
    val argsSeq = immutable.Seq(args).flatten
    functionName <> group(parens(nest(linebreak <> ssep(argsSeq, comma <> line)) <> linebreak))
  }

  def comment(doc: Doc): Doc = {
    "/*" <+> doc <+> "*/"
  }

  def arrayLiteral(items: Traversable[Doc]): Doc = {
    brackets(ssep(immutable.Seq(items.toSeq: _*), comma <> space))
  }

  def mapLiteral(map: Map[Doc, Doc]): Doc = {
    val entries = map.toSeq.sortBy(x => pretty(x._1)).map { case (k, v) => k <> ":" <+> v}
    braces(space <> group(nest(linebreak <> ssep(immutable.Seq(entries: _*), comma <> line)) <> line))
  }

  /**
   * Translate an expression into a Javascript lambda function.
   */
  def genLambda(expr: Expr, parameterNames: List[String], exprParameterNames: Option[List[String]] = None): Doc = {
    "function" <> parens(parameterNames.map(text).reduce(_ <> comma <+> _)) <+> braces {
      space <> "return" <+> genExpr(expr, exprParameterNames.getOrElse(parameterNames)) <> semi <+>
        comment(BloomPrettyPrinter.pretty(expr)) <> space
    }
  }

  /**
   * Translate expressions to statements appearing in UDF bodies.
   *
   * @param expr the expression to translate
   * @param parameterNames a list of the UDF's parameter names.
   */
  def genExpr(expr: Expr, parameterNames: List[String]): Doc = {
    expr match {
      case cr: CollectionRef =>
        parameterNames(cr.lambdaArgNumber)
      case BoundFieldRef(cr, field, _) =>
        parameterNames(cr.lambdaArgNumber) <> brackets(cr.collection.indexOfField(field).toString)
      case PlusStatement(a, b, _) =>
        genExpr(a, parameterNames) <+> plus <+> genExpr(b, parameterNames)
      case RowExpr(colExprs) =>
        arrayLiteral(colExprs.map(genExpr(_, parameterNames)))
    }
  }
}
