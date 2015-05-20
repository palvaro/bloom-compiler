package edu.berkeley.cs.boom.bloomscala

class TyperSuite extends BloomScalaSuite {

  test("NotIn schemas should match") {
    intercept[CompilerException] {
      compileSource(
        """
          | table a, [val: int]
          | table b, [val: string]
          | a <= a.notin(b)
        """.stripMargin)
    }
    emitter.assertContainFuzzy("[4.7] notin called with incompatible schemas:\n[int]\n[string]")
  }

  test("NotIn LHS and RHS schemas should match") {
    intercept[CompilerException] {
      compileSource(
        """
          | table a, [val: int]
          | table b, [val: int]
          | table c, [val: string]
          | c <= a.notin(b)
        """.stripMargin)
    }
    emitter.assertContainFuzzy("has wrong schema; expected [string] but got [int]")
  }

  test("Typing 3-way join") {
    compileSource(
      """
        | table a, [val: int]
        | table b, [val: int]
        | table c, [val: int]
        | c <= (a * b * c) on (a.val == b.val, b.val == c.val) { |x, y, z| [x.val + y.val + z.val] }
      """.stripMargin)
  }

  test("argmin ordering type must unify with ordering field type") {
    intercept[CompilerException] {
      compileSource(
        """
          | table a, [key: int, val: int]
          | table b, [key: int, val: int]
          | b <= a.argmin([a.key], a.val, stringOrder)
        """.stripMargin)
    }
    emitter.assertContainFuzzy("[4.7] expected partial order over int, but found function 'stringOrder' of type (string, string) -> boolean")
  }

  test("cannot insert into inputs") {
    intercept[CompilerException] {
      compileSource(
        """
          | input a, [key: int, val: int]
          | table b, [key: int, val: int]
          | a <= b
        """.stripMargin)
    }
    emitter.assertContainFuzzy("[4.2] Cannot insert into collections of type 'Input'")
  }

  test("cannot read from outputs") {
    intercept[CompilerException] {
      compileSource(
        """
          | output a, [key: int, val: int]
          | table b, [key: int, val: int]
          | b <= a
        """.stripMargin)
    }
    emitter.assertContainFuzzy("[4.2] Output collections cannot appear in the RHS of rules")
  }

  test("outputs only support asynchronous merge") {
    intercept[CompilerException] {
      compileSource(
        """
          | output a, [key: int, val: int]
          | table b, [key: int, val: int]
          | a <= b
        """.stripMargin)
    }
    emitter.assertContainFuzzy("[4.2] Output collections only support the <~ operator")
  }
}