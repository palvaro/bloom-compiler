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

  test("including modules should work as in BUD") {
    Compiler.compileToIntermediateForm(
      """
        |module Eggs {
        |    state {
        |        table foo, [:key]
        |        table bar, [:key]
        |    }
        |}
        |
        |module TastyNest {
        |    include Eggs
        |    bloom {
        |        foo <= bar
        |    }
        |}
        |
        |include TastyNest
        |table baz, [:key]
        |baz <= foo
      """.stripMargin
    )
  }

    test("importing modules with aliases should work as in BUD") {
      Compiler.compileToIntermediateForm(
        """
          |module Eggs {
          |    state {
          |        table foo, [:key]
          |        table bar, [:key]
          |    }
          |}
          |
          |module TastyNest {
          |    include Eggs
          |    bloom {
          |        foo <= bar
          |    }
          |}
          |
          |module Yum {
          | import TastyNest => :nest
          |
          | state {
          |   table baz, [:key]
          | }
          |
          | bloom {
          |   baz <= nest->foo{|f| [f.key]}
          |   nest->foo <= (baz * nest->bar) on (baz.key == nest->bar.key) { |b, r| r}
          | }
          |}
          |
          |module DoggieBag {
          | import Yum => yum
          |}
          |
          |include DoggieBag
        """.stripMargin
      )
    }

  test("nested records should work as in BUD") {
    Compiler.compileToIntermediateForm(
      """
        |module Eggs {
        |    state {
        |        table foo, [key, val: record]
        |        table baz, [key, val: record]
        |        table bar, [key, val]
        |    }
        |}
        |
        |module TastyNest {
        |    include Eggs
        |    bloom {
        |        //foo <= bar{|b| [b.key, b]}
        |        baz <= foo
        |    }
        |}
        |
        |include TastyNest
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
