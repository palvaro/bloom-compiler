package edu.berkeley.cs.boom.bloomscala

import org.scalatest.{Matchers, BeforeAndAfterEach, FunSuite}
import org.kiama.util.StringEmitter

class BloomScalaSuite extends FunSuite with BeforeAndAfterEach with Matchers {
  protected implicit val emitter = new StringEmitter

  override def afterEach() {
    emitter.clear()
  }
}
