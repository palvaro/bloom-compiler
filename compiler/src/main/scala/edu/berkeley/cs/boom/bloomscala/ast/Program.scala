package edu.berkeley.cs.boom.bloomscala.ast

import sext._


case class Program(nodes_in: Traversable[Node]) extends Node {
  lazy val declarations: Traversable[CollectionDeclaration] =
    (nodes.filter(_.isInstanceOf[CollectionDeclaration]).map(_.asInstanceOf[CollectionDeclaration]) ++
      nodes.filter(_.isInstanceOf[Program]).map(_.asInstanceOf[Program].declarations).flatten.toList.distinct)



  lazy val statements: Traversable[Statement] =
    nodes.filter(_.isInstanceOf[Statement]).map(_.asInstanceOf[Statement]) ++
      nodes.filter(_.isInstanceOf[Program]).map(_.asInstanceOf[Program].statements).flatten

  lazy val nodes: Traversable[Node] =
    nodes_in ++ nodes_in.filter(_.isInstanceOf[Program]).map(_.asInstanceOf[Program].nodes).flatten
}