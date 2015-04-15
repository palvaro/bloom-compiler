package edu.berkeley.cs.boom.bloomscala


class NamerSuite extends BloomScalaSuite {

  test("table aliases in the RHS should work as in BUD") {
    Compiler.compileToIntermediateForm(
      """
        | table lhs, [key]
        | table rhs, [val]
        | lhs <= rhs{|r| r}
      """.stripMargin
    )
  }

  test("join aliases in the RHS should work as in BUD") {
    Compiler.compileToIntermediateForm(
      """
        | table lhs, [key]
        | table rhs, [val]
        | lhs <= (lhs * rhs) on (lhs.key == rhs.val) {|l, r| r}
      """.stripMargin
    )
  }

  test("Referencing undeclared collections should fail") {
    intercept[CompilerException] { Compiler.compileToIntermediateForm("lhs <= rhs") }
  }

  test("Referencing non-tuple variables in map functions should fail") {
    intercept[CompilerException] { Compiler.compileToIntermediateForm(
      """
        | table apple, [val: int]
        | apple <= apple { |a| [apple.int] }
      """.stripMargin)
    }
  }

}
