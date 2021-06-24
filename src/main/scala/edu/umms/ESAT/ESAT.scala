package edu.umms.ESAT

import edu.umms.ESAT.parse
import org.apache.log4j.{BasicConfigurator, LogManager, Logger}
import scopt.OParser

object ESAT {
  /**
   * Start of ESAT program.
   * @param args program arguments.
   */
  def main(args: Array[String]) = {
    // Setup basic configuration for logger
    BasicConfigurator.configure()
    // Get logger
    val logger: Logger = LogManager.getLogger(this.getClass.getName)
    // Go parse input
    parse.Parser.doParse(args) match {
      case Some(params) =>
        val startTime = System.nanoTime()
        // Parse was successful - now go do the work
        println(s"Parsed $params")
        val timeSpent =
        logger.info(s"Total processing time: ${(System.nanoTime() - startTime )/ 1e9} sec\n")
      case _ =>
        // Parse failed, error message will have been displayed
        System.exit(1)
    }
  }
}
