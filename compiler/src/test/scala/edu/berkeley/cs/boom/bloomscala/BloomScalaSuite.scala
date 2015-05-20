package edu.berkeley.cs.boom.bloomscala

import edu.berkeley.cs.boom.bloomscala.ast.Program
import org.scalatest.{Matchers, BeforeAndAfterEach, FunSuite}
import org.kiama.util.{Emitter, StringEmitter}

class BloomScalaSuite extends FunSuite with BeforeAndAfterEach with Matchers {

  class ListEmitter extends Emitter {
    private var collectedMessages: List[String] = Nil

    def emit(any: Any): Unit = {
      collectedMessages = any.toString :: collectedMessages
    }

    def emitln(any: Any): Unit = {
      collectedMessages = any.toString :: collectedMessages
    }

    def emitln(): Unit = {}

    def clear(): Unit = {
      collectedMessages = Nil
    }

    def messages = collectedMessages.reverse

    def dump(): Unit = {
      messages.foreach(println)
    }

    def assertContainFuzzy(partialExpectedMessage: String): Unit = {
      assert(messages.exists(m => m.contains(partialExpectedMessage)),
        "Cant find message containting: " + partialExpectedMessage)
    }
  }

  protected implicit val emitter = new ListEmitter

  override def afterEach() {
    emitter.clear()
  }

  protected def compileSource(programSource: String): Program = {
    Compiler.compileToIntermediateForm(programSource)
  }

}