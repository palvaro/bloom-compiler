package edu.berkeley.cs.boom.bloomscala.exe

import edu.berkeley.cs.boom.bloomscala.analysis.Stratum
import edu.berkeley.cs.boom.bloomscala.ast.Program
import jnr.ffi.LibraryLoader
import edu.berkeley.cs.boom.bloomscala.codegen.c4.C4CodeGenerator



class C4Wrapper(name: String, program: String, maxstrat: Stratum, port: Int = 0) {
  //private val time = metrics.timer("time")
  C4Wrapper.libC4.c4_initialize()
  private val c4 = C4Wrapper.libC4.c4_make(null, port)
  private var budTime = 0

  def start: Unit = C4Wrapper.synchronized {
    try {
      assert(C4Wrapper.libC4.c4_install_str(c4, program) == 0)
      install("define(del_time, {int});")
      //wrap.install("delete stratum(X) :- del_stratum(X);")
      install("define(real_stratum, {int, int});")
      install("stratum(X) :- real_stratum(T, X), notin del_time(T);")

      install("define(next_strat, {int});")
      install("next_strat(X+1) :- stratum(X);")
    } finally {
     // C4Wrapper.libC4.c4_destroy(c4)
     // C4Wrapper.libC4.c4_terminate()
    }
  }

  def install(deltas: String): Unit = C4Wrapper.synchronized {
    try {
      println(s"INSTALL: *$deltas*")
      assert(C4Wrapper.libC4.c4_install_str(c4, deltas) == 0)
    }
  }

  def dump(tab: String) = C4Wrapper.synchronized {
    try {
      parseTableDump(C4Wrapper.libC4.c4_dump_table(c4, tab))
    }
  }

  def tick: Unit = {
    if (budTime > 0) {
      install(s"del_time(${budTime - 1});")
    }
    install(s"real_stratum($budTime, 0);")
    println(s"maxStrat is $maxstrat")
    var nextStrat = 1
    while (nextStrat < maxstrat.underlying) {
      println(s"so stratum $nextStrat upto ${Stratum.lastStratum.underlying}")
      install(s"real_stratum($budTime, $nextStrat);")
      // assuming that this is a barrier!
      val shouldBe = dump("next_strat")(0)(0).toInt
      println(s"next should be $shouldBe")
      nextStrat += 1
    }

    install(s"real_stratum($budTime, ${Stratum.lastStratum.underlying});")
    budTime += 1
  }

  def parseTableDump(string: String): List[List[String]] = {
    string.lines.map(_.split(",").toList).toList
  }
}

object C4Wrapper {
  val libC4: C4 = LibraryLoader.create(classOf[C4]).load("c4")
}
