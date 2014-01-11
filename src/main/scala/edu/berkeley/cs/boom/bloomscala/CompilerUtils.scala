package edu.berkeley.cs.boom.bloomscala

import scala.util.parsing.input.Position

trait CompilerUtils {

  def ensure(pred: Boolean, msg: => String, pos: Position) {
    if (!pred) fail(msg)(pos)
  }

  def fail(msg: String)(implicit pos: Position): Nothing = {
    throw new CompilerException(s"${pos.line}:${pos.column} $msg:\n${pos.longString}")
  }
}