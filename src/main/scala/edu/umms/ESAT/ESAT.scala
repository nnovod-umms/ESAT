package edu.umms.ESAT

import edu.umms.ESAT.parse
import edu.umms.ESAT.parse.Params
import org.apache.log4j.{BasicConfigurator, LogManager, Logger}
import scopt.OParser

import java.io.File
import scala.io.Source

object ESAT {
  opaque type Error = String
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
        println(s"$params")
        println(s"${getInput(params)}")
        logger.info(s"Total processing time: ${(System.nanoTime() - startTime )/ 1e9} sec\n")
      case _ =>
        // Parse failed, error message will have been displayed
        System.exit(1)
    }
  }

  private def canRead(file: String): Option[Error] =
    try {
      canRead(File(file))
    } catch {
      case e =>
        Some(s"Error accessing file $file: ${e.getLocalizedMessage}")
    }

  private def canRead(file: File): Option[Error] = {
    val path = file.getCanonicalPath
    if (!file.exists())
      Some(s"$path not found")
    else if (!file.isFile)
      Some(s"$path not a file")
    else if (file.canRead)
      None
    else
      Some(s"$path not accessible")
  }

  private def getInput(params: Params): Map[String, List[File]] | Error = {

    def parseAlignments(lines: Iterator[String]): (Map[String, List[File]], List[Error]) = {
      // Get list of experimentName->file (saving errors along the way)
      val (files, errs) = lines.foldLeft((List.empty[(String, File)], List.empty[Error])) {
          case ((filesSoFar, errsSoFar), line) =>
            // Trim line of leading and trailing blank space
            val trimmedLine = line.trim
            if (trimmedLine.isEmpty || trimmedLine.startsWith("#"))
              (filesSoFar, errsSoFar)
            else {
              // Parse line that should be experimentName<tab>fileName
              val lineContents = trimmedLine.split('\t')
              if (lineContents.size != 2)
                (filesSoFar,
                  errsSoFar :+ s"Invalid line in alignment file (should be experimentName<tab>fileName): $trimmedLine")
              else {
                val (expName, inFile) = (lineContents(0), lineContents(1))
                // Make sure file is accessible and add it to list
                canRead(inFile) match {
                  case Some(err) =>
                    (filesSoFar, errsSoFar :+ err)
                  case None =>
                    (filesSoFar :+ (expName -> File(inFile)), errsSoFar)
                }
              }
            }
      }
      // Group file by experiment name and return map of experimentName->files
      val filesByExp = files.groupBy {
        case (exp, _) => exp
      }.map {
        case (exp, files) =>
          exp -> files.map(_._2)
      }
      (filesByExp, errs)
    }

    // If list of input files then check we can read them all and return map of expermientName -> files
    if (params.inFiles.nonEmpty) {
      // If any files can't be read return error, otherwise return wanted map
      params.inFiles.flatMap(canRead) match {
        case Seq() => Map(params.inExperiment -> params.inFiles.toList)
        case errs => errs.mkString("\n")
      }
    } else {
      // Input files must be set in alignments file with format
      // experimentName<tab>inputFile
      params.alignments match {
        case Some(alignmentFile) =>
          // Check if alignment file accessible
          canRead(alignmentFile) match {
            case Some(err) =>
              err
            case None =>
              // Go read in alignment file
              val source = Source.fromFile(alignmentFile)
              try {
                val lines = source.getLines()
                // Get map of experimentName->files
                val (files, errs) = parseAlignments(lines)
                if (errs.nonEmpty)
                  errs.mkString("\n")
                else
                  files
              } finally {
                source.close()
              }
          }
        case _ =>
          s"No input files found"
      }
    }
  }
}
