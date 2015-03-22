package edu.berkeley.cs.boom.bloomscala.rewriting

import edu.berkeley.cs.boom.bloomscala.ast._

import edu.berkeley.cs.boom.bloomscala.analysis.Stratifier
import edu.berkeley.cs.boom.bloomscala.typing._

object StratRewrites {

  val dec = CollectionDeclaration(CollectionType.Table, "stratum", List(Field("stratum", FieldType.BloomInt)),List())

  /* TODO
  def hideChannels(program: Program): Program = {
    val nodes = program.nodes.map{ prog =>
        prog match {
          case CollectionDeclaration()
        }

    }
  }
  */

  def staggerNonmonotonics(program: Program): Program = {
    val nodes = program.nodes.flatMap{
      case d: CollectionDeclaration => List(d)
      case Statement(lhs, op, rhs, n) =>
        rhs match {
          case NotIn(left, anti) =>
            val nm = List(lhs.name, n, "_gated").mkString("_")
            val gate = CollectionDeclaration(CollectionType.Scratch, nm, left.collection.keys, left.collection.values)
            val gateRef = BoundCollectionRef(nm, gate, 0)
            List(
              gate,
              Statement(gateRef, op, rhs, n),
              Statement(lhs, BloomOp.InstantaneousMerge, gateRef, -1)
            )
          case _ => List(Statement(lhs, op, rhs, n))
        }
    }
    Program(nodes)
  }

  def addStratConditions(program: Program, stratifier: Stratifier): Program = {
    def addStratPred(rhs: StatementRHS, stratum: Int): StatementRHS = {

      def toJoin(collection: Node): JoinedCollections = {
        val newSubgoal = BoundCollectionRef("stratum", dec, 1)
        val newPredicate = EqualityPredicate(BoundFieldRef(newSubgoal, "stratum", Field("stratum", FieldType.BloomInt)), ConstantColExpr(stratum.toString, FieldType.BloomInt))
        collection match {
          case MappedCollection(c, t, r) => JoinedCollections(List(c, newSubgoal), List(newPredicate), t, r)
          case cr: CollectionRef =>
            val fieldNames = (cr.collection.keys ++ cr.collection.values)
            val expr = RowExpr(fieldNames.map(v => BoundFieldRef(cr, v.name, Field(v.name, v.typ))).toList)
            JoinedCollections(List(cr, newSubgoal), List(newPredicate), List(cr.name), expr)
        }
      }

      rhs match {
        case JoinedCollections(collections, predicates, tupleVars, rowExpr) =>
          val newSubgoal = BoundCollectionRef("stratum", dec, collections.length)
          val newPredicate = EqualityPredicate(BoundFieldRef(newSubgoal, "stratum", Field("stratum", FieldType.BloomInt)), ConstantColExpr(stratum.toString, FieldType.BloomInt))
          JoinedCollections(collections ++ List(newSubgoal), predicates ++ List(newPredicate), tupleVars, rowExpr)
        case cr: MappedCollection => toJoin(cr)
        case cr: CollectionRef => toJoin(cr)
        case _ => rhs

      }
    }

    val nodes = program.nodes.map {
      case d: CollectionDeclaration => d
      case s: Statement => Statement(s.lhs, s.op, addStratPred(s.rhs, stratifier.ruleStratum(s).underlying))
    }
    Program(Seq(dec) ++ nodes)
  }

}
