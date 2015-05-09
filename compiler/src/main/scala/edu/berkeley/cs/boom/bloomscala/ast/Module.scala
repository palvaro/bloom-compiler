package edu.berkeley.cs.boom.bloomscala.ast


case class Module(nodes: Traversable[Node], name: String) extends Node {
  lazy val declarations: Traversable[CollectionDeclaration] =
    nodes.filter(_.isInstanceOf[CollectionDeclaration]).map(_.asInstanceOf[CollectionDeclaration])
  lazy val statements: Traversable[Statement] =
    nodes.filter(_.isInstanceOf[Statement]).map(_.asInstanceOf[Statement])
}