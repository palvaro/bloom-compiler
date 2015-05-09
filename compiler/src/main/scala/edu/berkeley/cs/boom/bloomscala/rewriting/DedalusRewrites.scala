package edu.berkeley.cs.boom.bloomscala.rewriting

import edu.berkeley.cs.boom.bloomscala.ast._

import edu.berkeley.cs.boom.bloomscala.analysis.Stratifier
import edu.berkeley.cs.boom.bloomscala.typing._

object DedalusRewrites {

  def frameRules(program: Program): Program = {
    val decs = program.declarations.flatMap {
      case CollectionDeclaration(CollectionType.Table, name, keys, vals) =>
        // not crazy about this repetition either; why can't I alias and pattern match?
        val ldec = CollectionDeclaration(CollectionType.Table, name, keys, vals)
        val nm = "del_" +name
        val rdec = CollectionDeclaration(CollectionType.Table, nm, keys, vals)
        val lhs = BoundCollectionRef(name, ldec, 0)
        val rhs1 = BoundCollectionRef(name, ldec, 1)
        val rhs2 = BoundCollectionRef(nm, rdec, 2)
        List(
          CollectionDeclaration(CollectionType.Table, name, keys, vals),
          CollectionDeclaration(CollectionType.Scratch, nm, keys, vals),
          Statement(lhs, BloomOp.DeferredMerge, NotIn(rhs1, rhs2))
        )
      case n => List(n)
    }
    Program(program.nodes ++ decs)
  }
}
