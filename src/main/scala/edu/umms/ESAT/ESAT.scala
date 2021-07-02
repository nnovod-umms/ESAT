package edu.umms.ESAT

import edu.umms.ESAT.parse
import edu.umms.ESAT.parse.Params
import edu.umms.ESAT.utils.{Control, Files, ReaderWithHeader, Types}
import edu.umms.ESAT.utils.Types.ErrorStr
import edu.umms.ESAT.bio.Gene
import org.apache.log4j.{BasicConfigurator, LogManager, Logger}
import scopt.OParser

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, SortedSet, TreeMap}
import scala.io.Source

object ESAT {
  // Get logger
  lazy val logger: Logger = LogManager.getLogger(this.getClass.getName)
  /**
   * Start of ESAT program.
   * @param args program arguments.
   */
  def main(args: Array[String]) = {
    // Setup basic configuration for logger
    BasicConfigurator.configure()
    // Go parse input
    parse.Parser.doParse(args) match {
      case Some(params) =>
        val startTime = System.nanoTime()
        // Parse was successful - now go do the work
        println(s"$params")
        println(s"${getInput(params)}")
        println(s"${parseGeneMapping(File("NoWay"))}")
        val g1 = Gene("a", 1, 5, "b", "+", List(1, 2, 3, 4), List(2, 3, 4, 5))
        val g2 = Gene("a", 1, 5, "b", "+", List(1, 2, 3, 4), List(2, 3, 4, 5))
        println(g1.mergeExons(g2))
        val g3 = Gene("a", 1, 25, "b", "+", List(1, 20, 5, 10), List(2, 25, 10, 19))
        val g4 = Gene("a", 1, 50, "b", "+", List(1, 30, 10, 40), List(7, 40, 11, 50))
        println(g4.copy(isoForms = SortedSet(g1, g2, g3, g4)))
        println(g3.mergeExons(g4))
        logger.info(s"Total processing time: ${(System.nanoTime() - startTime )/ 1e9} sec\n")
      case _ =>
        // Parse failed, error message will have been displayed
        System.exit(1)
    }
  }

  /**
   * Get input bam files.
   * @param params input parameters
   * @return map of experimentName->bamFiles
   */
  private def getInput(params: Params): Map[String, List[File]] | ErrorStr = {

    // If list of input files then check we can read them all and return map of expermientName -> files
    if (params.inFiles.nonEmpty) {
      // If any files can't be read return error, otherwise return wanted map
      params.inFiles.flatMap(Files.canRead) match {
        case Seq() => Map(params.inExperiment -> params.inFiles.toList)
        case errs => Types.error(errs.mkString("\n"))
      }
    } else {
      // Input files must be set in alignments file with format
      // experimentName<tab>inputFile
      params.alignments match {
        case Some(alignmentFile) =>
          // Check if alignment file accessible
          Files.canRead(alignmentFile) match {
            case Some(err) =>
              err
            case None =>
              // Go read in alignment file
              Control.using(Source.fromFile(alignmentFile)) {
                source =>
                  val lines = source.getLines()
                  // Get map of experimentName->files
                  val (files, errs) = parseAlignments(lines)
                  if (errs.nonEmpty)
                    Types.error(errs.mkString("\n"))
                  else
                    files
              }
          }
        case _ =>
          Types.error(s"No input files found")
      }
    }
  }

  /**
   * Parse alignment file with lines:
   * experimentName/tab/bamFileSpecification
   * If any lines have an invalid format or files can't be accessed an error is returned.
   * @param lines alignment file input
   * @return (map of experimentName->bamFiles, listOfFileAccessErrors)
   */
  private def parseAlignments(lines: Iterator[String]): (Map[String, List[File]], List[ErrorStr]) = {
    // Get list of experimentName->file (saving errors along the way)
    val (files, errs) = lines.foldLeft((List.empty[(String, File)], List.empty[ErrorStr])) {
      case ((filesSoFar, errsSoFar), line) =>
        // Trim line of leading and trailing blank space
        val trimmedLine = line.trim
        // Ignore comment (lines starting with #) or blank lines
        if (trimmedLine.isEmpty || trimmedLine.startsWith("#"))
          (filesSoFar, errsSoFar)
        else {
          // Parse line that should be experimentName<tab>fileName
          val lineContents = trimmedLine.split('\t')
          if (lineContents.size != 2 || lineContents(0).isEmpty || lineContents(1).isEmpty)
            (filesSoFar,
              errsSoFar :+ Types.error(
                s"Invalid line in alignment file (should be experimentName<tab>fileName): $trimmedLine"
              )
            )
          else {
            val (expName, inFile) = (lineContents(0), lineContents(1))
            // Make sure file is accessible and add it to list (add to error list if not accessible)
            Files.canRead(inFile) match {
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

  /**
   * Parse gene mapping file.
   * @param file Gene mapping file
   * @return TreeMap(chr -> SortedSet[Gene]) or ErrorStr
   */
  private def parseGeneMapping(file: File): TreeMap[String, SortedSet[Gene]] | ErrorStr = {
    // Mapping file headers
    val geneMappingHeaders =
      Array("name2", "chrom", "txStart", "txEnd", "name", "strand", "exonStarts", "exonEnds")
    // Make symbols from headers
    val (
      geneSymbol: String, chrom: String,
      txStart: String, txEnd: String, txID: String,
      strand: String, exonStarts: String, exonEnds: String
    ) = Tuple.fromArray(geneMappingHeaders)
    // Go read in file to make map of genes found
    val chrTreeWithGenes =
      ReaderWithHeader.foldFile(
        file = file, headersNeeded = geneMappingHeaders, init = TreeMap.empty[String, Map[String, Gene]], sep = "\t"
      ) {
        (soFar, newLine, headerIndicies) =>
          // Some little helper methods
          @inline
          def getField(fieldName: String): String = newLine(headerIndicies(fieldName))
          @inline
          def getIntList(fieldName: String) =
            getField(fieldName).split(",").map(_.toInt).toList

          // Get chromosome (key for Treemap)
          val chr = getField(chrom)
          // Get geneName (key for map of Genes)
          val geneName = getField(geneSymbol)
          // Make new Gene object
          // @TODO name set to transcript ID.  Unclear if that or geneName is wanted.  In old ESAT it's first set
          // to txID but then always changed to geneName.  ESAT also sets isoForms with extra entries that match, but
          // getIsoforms method gets both extra entries and original entry, but ESAT itself sets the isoForms in
          // combined entries to be the original entry plus others (so getIsoforms will get back original entry
          // twice).  In a word, isoForms is a mess but maybe we don't even need it.  Also there's a bug in the
          // Java version of Gene that sets both start and end to start when Gene entries were merged (via union) so
          // it's unclear how important start/end is.
          val newGene =
            Gene(
              chr = chr,
              start = getField(txStart).toInt, end = getField(txEnd).toInt,
              name = getField(txID), orientation = getField(strand),
              exonStarts = getIntList(exonStarts), exonEnds = getIntList(exonEnds)
            )

          // See if chromsome already in tree
          soFar.get(chr) match {
            // New chromosome - init it with map of new entry
            case None =>
              val topGene = newGene.copy(name = geneName, isoForms = SortedSet(newGene))
              soFar + (chr -> Map(geneName -> topGene))
            case Some(chrEntries) =>
              // Chrosome already there - Look for gene
              chrEntries.get(geneName) match {
                // If gene already there then add new entry with merge of previous genes
                case Some(foundGene) =>
                  if (newGene.chr == foundGene.chr && newGene.orientation == foundGene.orientation) {
                    val (newStart, newEnd, newExons) = foundGene.mergeExons(newGene)
                    val startToSet = if (newExons.isEmpty) foundGene.start else newStart
                    val endToSet = if (newExons.isEmpty) foundGene.end else newEnd
                    val isoForms = foundGene.isoForms + newGene
                    val mergedGene =
                      foundGene.copy(start = startToSet, end = endToSet, exons = newExons, isoForms = isoForms)
                    soFar + (chr -> (chrEntries + (geneName -> mergedGene)))
                  } else {
                    // If not same orientation then ignore it and issue a warning
                    logger.warn(s"Isoform mismatch found for $geneName (${foundGene.chr}${foundGene.orientation}) with "+
                      s"${newGene.name} (${newGene.chr}${newGene.orientation})")
                    soFar
                  }
                // If new gene then add new entry into gene map
                case None =>
                  val topGene = newGene.copy(name = geneName, isoForms = SortedSet(newGene))
                  soFar + (chr -> (chrEntries + (geneName -> topGene)))
              }
          }
      }

    // Finish up by converting the gene map into a sorted set
    chrTreeWithGenes match {
      // Successfully made TreeMap
      case _ : TreeMap[_, _] =>
        val t = chrTreeWithGenes.asInstanceOf[TreeMap[String, Map[String, Gene]]]
        t.map {
          case (key, value) => key -> SortedSet(value.values.toSeq:_*)
        }
      // Error
      case e => e.asInstanceOf[ErrorStr]
    }
  }

  /* If we don't build TreeMap in first pass...
  private def GeneMappingToAnnotation(genes: Map[String, Gene]): TreeMap[String, SortedSet[Gene]] = {
    val byChr = genes.groupBy(_._2.chr).map{
      case (chr, genes) => chr -> SortedSet(genes.values.toSeq:_*)
    }
    TreeMap(byChr.toSeq:_*)
  }
  */
}
