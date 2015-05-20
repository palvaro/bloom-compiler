package edu.berkeley.cs.boom.bloomscala

class NamerSuite extends BloomScalaSuite {

  test("table aliases in the RHS should work as in BUD") {
    compileSource(
      """
        | table lhs, [key]
        | table rhs, [val]
        | lhs <= rhs{|r| r}
      """.stripMargin
    )
  }

  test("join aliases in the RHS should work as in BUD") {
    compileSource(
      """
        | table lhs, [key]
        | table rhs, [val]
        | lhs <= (lhs * rhs) on (lhs.key == rhs.val) {|l, r| r}
      """.stripMargin
    )
  }

  test("including modules should work as in BUD") {
    compileSource(
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
    compileSource(
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

  test("Referencing undeclared collections on right side should fail") {
    intercept[CompilerException] {
      compileSource("table lhs, [val]\nlhs <= rhs")
    }
    emitter.assertContainFuzzy("[2.8] Unknown collection rhs")
  }

  test("Referencing undeclared collections on left side should fail") {
    intercept[CompilerException] {
      compileSource("table there, [val]\nnotThere <= there")
    }
    emitter.assertContainFuzzy("[2.1] Unknown collection notThere")
  }

  test("nested records should work as in BUD") {
    compileSource(
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
    intercept[CompilerException] {
      compileSource("lhs <= rhs")
    }
    emitter.assertContainFuzzy("[1.1] Unknown collection lhs")
    emitter.assertContainFuzzy("[1.8] Unknown collection rhs")
  }

  test("Referencing non-tuple variables in map functions should fail") {
    intercept[CompilerException] {
      compileSource(
        """
          | table apple, [val: int]
          | apple <= apple { |a| [apple.int] }
        """.stripMargin)
    }
    emitter.assertContainFuzzy("[3.24] Collection apple does not have field int")
  }

  test("Referencing undeclared module should fail") {
    intercept[CompilerException] {
      compileSource(
        """
          | module Other {
          | }
          |include This
        """.stripMargin)
    }
    emitter.assertContainFuzzy("Couldn't find module This among List(Other)")
  }
}
