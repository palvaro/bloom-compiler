package edu.berkeley.cs.boom.bloomscala.ast

import edu.berkeley.cs.boom.bloomscala.typing.{FieldType, UnknownType, RecordType, BloomType}
import edu.berkeley.cs.boom.bloomscala.ast.Expr

/************************* Base Classes ***********************************/

sealed trait Expr extends Node {
  val typ: BloomType
}

/**
 * An expression producing a scalar value.
 */
trait ColExpr extends Expr

/**
 * An expression producing a fixed-size array with heterogeneous element types.
 */

/*
trait AbstractRowExpr() extends Expr {
  val cols: List[ColExpr] = List()
  val typ: BloomType = RecordType(cols.map(_.typ))
}
*/

case class RowExpr(cols: List[ColExpr]) extends Expr {

  val typs = cols.map { r =>
    r.typ match {
      case FieldType("location") => FieldType.BloomString
      case t => t
    }
  }

  val typ: BloomType = RecordType(typs)
}


/*************************** Arithmetic ***********************************/

case class PlusStatement(lhs: ColExpr, rhs: ColExpr, override val typ: BloomType) extends ColExpr


/************************** Predicates ***********************************/

trait Predicate extends Node

case class EqualityPredicate(a: ColExpr, b: ColExpr) extends Predicate


/************************** Functions ***********************************/

case class FunctionCall(functionRef: FunctionRef, arguments: List[ColExpr]) extends ColExpr {
  override val typ = functionRef.function.typ.returnType
}

case class ConstantColExpr(data: String, override val typ: BloomType) extends ColExpr

case class TableRefExpr(alias: String) extends Expr {
  override val typ = UnknownType()
}