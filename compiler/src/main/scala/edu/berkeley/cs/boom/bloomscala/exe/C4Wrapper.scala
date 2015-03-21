package edu.berkeley.cs.boom.bloomscala.exe

//import edu.berkeley.cs.boom.molly.UltimateModel
//import edu.berkeley.cs.boom.molly.ast.{IntLiteral, Program}

import edu.berkeley.cs.boom.bloomscala.analysis.Stratum
import edu.berkeley.cs.boom.bloomscala.ast.{Program}
import jnr.ffi.LibraryLoader

//import com.typesafe.scalalogging.LazyLogging
import edu.berkeley.cs.boom.bloomscala.codegen.c4.C4CodeGenerator
//import nl.grons.metrics.scala.InstrumentedBuilder
//import com.codahale.metrics.MetricRegistry


class C4Wrapper(name: String, program: String, maxstrat: Stratum)
               //(implicit val metricRegistry: MetricRegistry)
              //extends LazyLogging with InstrumentedBuilder {
{
  //private val time = metrics.timer("time")
  C4Wrapper.libC4.c4_initialize()
  private val c4 = C4Wrapper.libC4.c4_make(null, 0)
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
    install(s"real_stratum($budTime, 0);")
    println(s"maxStrat is $maxstrat")
    //val ns = dump("next_strat")
    //println(s"next strat is $ns")
    //var nextStrat = ns(0)(0).toInt
    /*
    var nextStrat = 1;

    while (nextStrat < maxstrat.underlying) {
      println(s"so stratum $nextStrat upto ${Stratum.lastStratum.underlying}")
      install(s"stratum($nextStrat);")
      nextStrat = dump("next_strat")(0)(0).toInt
    }
    budTime += 1
    */
  }

  def parseTableDump(string: String): List[List[String]] = {
    string.lines.map(_.split(",").toList).toList
  }
}

object C4Wrapper {
  val libC4: C4 = LibraryLoader.create(classOf[C4]).load("c4")
}
