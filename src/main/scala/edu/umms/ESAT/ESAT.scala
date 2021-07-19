package edu.umms.ESAT

import edu.umms.ESAT.parse
import edu.umms.ESAT.parse.Params
import edu.umms.ESAT.utils.{Files, Reader, ReaderWithHeader, Types}
import edu.umms.ESAT.utils.Types.ErrorStr
import edu.umms.ESAT.bio.{Gene, GeneBEDAnnotation, GeneMapping, GeneFileFold}
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
  def main(args: Array[String]) =
    // Setup basic configuration for logger
    BasicConfigurator.configure()
    // Go parse input
    parse.Parser.doParse(args) match
      case Some(params) =>
        val startTime = System.nanoTime()
        // Parse was successful - now go do the work
        // Get input file along with processor (one of bed annotation and gene mapping file must be specified)
        val (geneParser: GeneFileFold, geneFile: File) =
          if (params.annotationFile.isDefined)
            (GeneBEDAnnotation, params.annotationFile.get)
          else
            (GeneMapping, params.gMapFile.get)
        // Parse input file
        val inputData = parseGeneInputData(geneFile, geneParser)
        inputData match
          // Successfully made TreeMap
          case _ : TreeMap[_, _] =>
            val t = inputData.asInstanceOf[TreeMap[String, SortedSet[Gene]]]
            TEMP_checkInput(t)
          // Error
          case e =>
            logger.error(s"Failure processing input: ${e.asInstanceOf[ErrorStr]}")
        end match

        logger.info(s"Total processing time: ${(System.nanoTime() - startTime )/ 1e9} sec\n")
      case _ =>
        // Parse failed, error message will have been displayed
        System.exit(1)
    end match
  end main

  private def TEMP_checkInput(t: TreeMap[String, SortedSet[Gene]]) =
    t.foreach {
      case (chr, geneMap) =>
        println(s"Chr: $chr:")
        println(s"  ${geneMap.size} gene(s)")
        val isos = geneMap.filter((g) => g.isoForms.size != 1)
        if (isos.nonEmpty)
          println(s"  Found ${isos.size} with multiple isoForms")
          isos.foreach(iso => {
            val iName = iso.name
            val diffName = iso.isoForms.filter(i => i.name != iName)
            if (diffName.nonEmpty)
              println(s"    ** ${diffName.size} with different gene names")
            val iRegions = iso.exons.size
            val totRegions = iso.isoForms.foldLeft(0)(_ + _.exons.size)
            if (totRegions != iRegions)
              println(s"    ** $iRegions merged into $totRegions")
          })
          isos.foreach(i => println(s"    $i"))
        end if
    }
  end TEMP_checkInput

  /**
   * Get input bam files.
   * @param params input parameters
   * @return map of experimentName->bamFiles
   */
  private def getInput(params: Params): Map[String, List[File]] | ErrorStr =
    // If list of input files then check we can read them all and return map of expermientName -> files
    if (params.inFiles.nonEmpty)
      // If any files can't be read return error, otherwise return wanted map
      params.inFiles.flatMap(Files.canRead) match
        case Seq() => Map(params.inExperiment -> params.inFiles.toList)
        case errs => Types.error(errs.mkString("\n"))
      end match
    else
      // Input files must be set in alignments file with format
      // experimentName<tab>inputFile
      params.alignments match
        case Some(alignmentFile) =>
          // Check if alignment file accessible
          Files.canRead(alignmentFile) match {
            case Some(err) =>
              err
            case None =>
              // Go read in alignment file
              Files.using(Source.fromFile(alignmentFile)) {
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
      end match
    end if
  end getInput

  /**
   * Parse alignment file with lines:
   * experimentName/tab/bamFileSpecification
   * If any lines have an invalid format or files can't be accessed an error is returned.
   * @param lines alignment file input
   * @return (map of experimentName->bamFiles, listOfFileAccessErrors)
   */
  private def parseAlignments(lines: Iterator[String]): (Map[String, List[File]], List[ErrorStr]) =
    // Get list of experimentName->file (saving errors along the way)
    val (files, errs) = lines.foldLeft((List.empty[(String, File)], List.empty[ErrorStr])) {
      case ((filesSoFar, errsSoFar), line) =>
        // Trim line of leading and trailing blank space
        val trimmedLine = line.trim
        // Ignore comment (lines starting with #) or blank lines
        if (trimmedLine.isEmpty || trimmedLine.startsWith("#"))
          (filesSoFar, errsSoFar)
        else
          // Parse line that should be experimentName<tab>fileName
          val lineContents = trimmedLine.split('\t')
          if (lineContents.size != 2 || lineContents(0).isEmpty || lineContents(1).isEmpty)
            (filesSoFar,
              errsSoFar :+ Types.error(
                s"Invalid line in alignment file (should be experimentName<tab>fileName): $trimmedLine"
              )
            )
          else
            val (expName, inFile) = (lineContents(0), lineContents(1))
            // Make sure file is accessible and add it to list (add to error list if not accessible)
            Files.canRead(inFile) match
              case Some(err) =>
                (filesSoFar, errsSoFar :+ err)
              case None =>
                (filesSoFar :+ (expName -> File(inFile)), errsSoFar)
            end match
          end if
        end if
    }
    // Group file by experiment name and return map of experimentName->files
    val filesByExp = files.groupBy {
      case (exp, _) => exp
    }.map {
      case (exp, files) =>
        exp -> files.map(_._2)
    }
    (filesByExp, errs)
  end parseAlignments

  /**
   * Parse gene input file.
   * @param file Gene mapping file
   * @param folder callback to parse/fold file input
   * @return TreeMap(chr -> SortedSet[Gene]) or ErrorStr
   */
  private def parseGeneInputData(file: File, folder: GeneFileFold): TreeMap[String, SortedSet[Gene]] | ErrorStr =
    // Go read in file to make map of genes found
    val chrTreeWithGenes =
      folder.foldGeneFile(file, TreeMap.empty[String, Map[String, Gene]]) {
        (soFar, chr, geneName, newGene) =>
          // See if chromsome already in tree
          soFar.get(chr) match
            // New chromosome - init it with map of new entry
            case None =>
              val topGene = newGene.copy(name = geneName, isoForms = SortedSet(newGene))
              soFar + (chr -> Map(geneName -> topGene))
            case Some(chrEntries) =>
              // Chromosome already there - Look for gene
              chrEntries.get(geneName) match
                // If gene already there then add new entry with merge of previous entries
                case Some(foundGene) =>
                  // Get min, treating 0 as unspecified
                  def getMin(x: Int, y: Int) =
                    if (x == 0)
                      y
                    else if (y == 0)
                      x
                    else
                      Integer.min(x, y)
                    end if
                  end getMin

                  if (newGene.chr == foundGene.chr && newGene.orientation == foundGene.orientation)
                    // Get start of found/new gene combination
                    val newStart = getMin(newGene.start, foundGene.start)
                    // Merge exons of two genes
                    val (newCdsStart, newCdsEnd, newExons) = foundGene.mergeExons(newGene)
                    val isoForms = foundGene.isoForms + newGene
                    val mergedGene =
                      foundGene.copy(start = newStart, end = Integer.max(newGene.end, foundGene.end),
                        exons = newExons, cdsStart = newCdsStart, cdsEnd = newCdsEnd, isoForms = isoForms)
                    soFar + (chr -> (chrEntries + (geneName -> mergedGene)))
                  else
                    // If not same orientation then ignore it and issue a warning
                    logger.warn(s"Isoform mismatch found for $geneName (${foundGene.chr}${foundGene.orientation}) with "+
                      s"${newGene.name} (${newGene.chr}${newGene.orientation})")
                    soFar
                  end if
                // If new gene then add new entry into gene map
                case None =>
                  val topGene = newGene.copy(name = geneName, isoForms = SortedSet(newGene))
                  soFar + (chr -> (chrEntries + (geneName -> topGene)))
              end match
          end match
      }

    // Finish up by converting the gene map into a sorted set
    chrTreeWithGenes match
      // Successfully made TreeMap
      case _ : TreeMap[_, _] =>
        val t = chrTreeWithGenes.asInstanceOf[TreeMap[String, Map[String, Gene]]]
        t.map {
          case (key, value) => key -> SortedSet(value.values.toSeq:_*)
        }
      // Error
      case e => e.asInstanceOf[ErrorStr]
    end match
  end parseGeneInputData


  /* If we don't build TreeMap in first pass...
  private def GeneMappingToAnnotation(genes: Map[String, Gene]): TreeMap[String, SortedSet[Gene]] = {
    val byChr = genes.groupBy(_._2.chr).map{
      case (chr, genes) => chr -> SortedSet(genes.values.toSeq:_*)
    }
    TreeMap(byChr.toSeq:_*)
  }
  */
}
