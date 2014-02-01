package edu.berkeley.cs.boom.bloomscala

import org.scalatest.FunSuite


class TyperSuite extends FunSuite {

  test("NotIn schemas should match") {
    intercept[CompilerException] { Compiler.compileToIntermediateForm(
      """
        | table a, [val: int]
        | table b, [val: string]
        | a <= a.notin(b)
      """.stripMargin)
    }
  }

  test("NotIn LHS and RHS schemas should match") {
    intercept[CompilerException] { Compiler.compileToIntermediateForm(
      """
        | table a, [val: int]
        | table b, [val: int]
        | table c, [val: string]
        | c <= a.notin(b)
      """.stripMargin)
    }
  }
}