package edu.berkeley.cs.boom.bloomscala.rewriting

import edu.berkeley.cs.boom.bloomscala.ast._

import edu.berkeley.cs.boom.bloomscala.analysis.Stratifier
import edu.berkeley.cs.boom.bloomscala.typing._

object C4Rewrites {

  def staggerChannels(program: Program): Program = {
    val nodes = program.nodes.flatMap{
      case d: CollectionDeclaration => List(d)
      case Statement(lhs, op, rhs, n) =>
        if (lhs.collection.collectionType == CollectionType.Channel || lhs.collection.collectionType == CollectionType.Output) {
          // do something to eliminate this repetition.
          val buf = List(lhs.name, n, "_buffer").mkString("_")
          val gate = CollectionDeclaration(CollectionType.Scratch, buf, lhs.collection.keys, lhs.collection.values)
          val gateRef = BoundCollectionRef(buf, gate, 0)
          List(
            gate,
            Statement(gateRef, op, rhs, n),
            Statement(lhs, BloomOp.InstantaneousMerge, gateRef, -1)
          )
        } else {
          List(Statement(lhs, op, rhs, n))
        }
      case m => List(m)
    }
    Program(nodes)
  }
}
