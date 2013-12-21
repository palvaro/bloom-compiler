package edu.berkeley.cs.boom.bloomscala.examples

import edu.berkeley.cs.boom.bloomscala.{Rule, Bud}
import edu.berkeley.cs.boom.bloomscala.collections.Table

object ShortestPaths {
  implicit val bud = new Bud()

  val link: Table[(Char, Char, Int)] = new Table[(Char, Char, Int)]
  // from, to, cost
  val path: Table[(Char, Char, Char, Int)] = new Table[(Char, Char, Char, Int)]
  // from, to, next, cost

  //val shortest: Table[(Char, Char, Char, Int)] = ???

  val j = link.join(path, x => x._2, (x: (Char, Char, Char, Int)) => x._1)


  val strata0Rules = Seq[Rule](
    link <= ('a', 'b', 1),
    link <= ('a', 'b', 3),
    link <= ('b', 'c', 1),
    link <= ('c', 'd', 1),
    link <= ('d', 'e', 1),
    path <= link.map {x => (x._1, x._2, x._2, x._3)},
    path <= j.map { case (l, p) =>
      (l._1, p._2, p._1, p._4 + l._3)
    }//,
    //shortest <= path
  )
  bud.addStrata(strata0Rules)

  def main(args: Array[String]): Unit = {
    bud.tick()
    link.foreach(println)
    println()
    path.foreach(println)
  }
}