package edu.berkeley.cs.boom.bloomscala

import com.typesafe.scalalogging.slf4j.Logging
import edu.berkeley.cs.boom.bloomscala.collections.BudCollection

class Stratum(rules: Iterable[Rule], tables: Iterable[BudCollection[_]]) extends Logging {
  def fixpoint() {
    val (oneShotRules, multiShotRules) = rules.partition(_.isInstanceOf[OneShotRule])
    oneShotRules.foreach(_.evaluate())
    var reachedFixpoint = false
    do {
      val counts = tables.map(_.size)
      logger.debug("Computing fixpoint")
      multiShotRules.foreach(_.evaluate())
      reachedFixpoint = tables.zip(counts).forall { case (table, count) => table.size == count }
    } while (!reachedFixpoint)
    logger.debug("Finished computing fixpoint")
  }
}
