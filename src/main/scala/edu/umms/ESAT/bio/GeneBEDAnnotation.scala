package edu.umms.ESAT.bio

import edu.umms.ESAT.utils.Reader
import edu.umms.ESAT.utils.Types.ErrorStr
import org.apache.log4j.{LogManager, Logger}

import java.io.File

object GeneBEDAnnotation extends GeneFileFold {
  // Get logger
  lazy private val logger: Logger = LogManager.getLogger(this.getClass.getName)

  // Make symbols for indicies to pieces of input line
  // Fields below must be in order they will appear within file input lines
  // See https://en.wikipedia.org/wiki/BED_(file_format)
  private val
  (
    chrom: Int, chromStart: Int, chromEnd: Int,
    geneSymbol: Int, bedScore: Int,
    orientation: Int, cdsStart: Int, cdsEnd: Int,
    displayColor: Int, numExons: Int,
    exonSizes: Int, exonStarts: Int
  ) = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

  /**
   * Fold BED annotation file into chosen object.
   * @param file BED annotation file
   * @param init initial value for fold
   * @param doFold callback to do fold of each line (foldedValueSoFar, chr, geneName, geneObject) => newFoldedValue
   * @tparam T type of folded value
   * @return final folded value
   */
  def foldGeneFile[T](file: File, init: T)(doFold: (T, String, String, Gene) => T): T | ErrorStr =
    // Method to trim and split line using white space as separator and returning empty array if comment or header line
    def splitLine(line: String) =
      if (line.isEmpty || line.startsWith("#") || line.startsWith("track") || line.startsWith("browser"))
        Array.empty[String]
      else
        line.split("\\s++")
      end if
    end splitLine

    // Go read in file to make map of genes found
    Reader.foldFile(file = file, init = init)(parseLine = splitLine) {
      (soFar, newLine) =>
        // Some little helper methods
        @inline
        def getField(fieldIndex: Int): Option[String] =
          if (newLine.isDefinedAt(fieldIndex))
            Some(newLine(fieldIndex))
          else
            None
          end if
        end getField

        @inline
        def getIntFields(fieldIndex: Int): Option[Array[Int]] =
          // @TODO replaceAll needed to get rid of quotes?  Was in setBlockStartsAndEnds of old Gene code
          getField(fieldIndex).map(_.replaceAll("\"", "").split(",").map(_.toInt))

        try
          // Get each field, continuing so long as fields retrieved (flatMap method only called for Some(val))
          getField(chrom).flatMap(chr =>
            getField(geneSymbol).flatMap(geneName =>
              getField(chromStart).flatMap(txStartStr =>
                getField(chromEnd).flatMap(txEndStr =>
                  getField(orientation).flatMap(strand =>
                    getField(numExons).flatMap(numberExons =>
                      getIntFields(exonSizes).flatMap(exonSizesArray =>
                        getIntFields(exonStarts).map(exonStartArray => {
                          // We retrieved all the fields needed - make a Gene and callback to fold it in
                          val start = txStartStr.toInt
                          val (exonStarts, exonEnds) =
                            getExonStartsAndEnds(exonStartOffsets = exonStartArray, exonSizes = exonSizesArray,
                              numExons = numberExons.toInt, blockStart = start)
                          val newGene = Gene(
                            chr = chr, start = start, end = txEndStr.toInt,
                            name = geneName, orientation = strand,
                            exonStarts = exonStarts, exonEnds = exonEnds
                          )
                          doFold(soFar, chr, geneName, newGene)
                        })
                      )
                    )
                  )
                )
              )
            )
          )
          match
            case Some(foldValue) => foldValue
            case None =>
              // If not blank line then something is not right
              if (newLine.nonEmpty)
                logger.error(s"Missing fields in BED annotation file line: ${newLine.mkString("\t")}")
              soFar
          end match
        catch
          case e =>
            logger.error(s"Error (${e.getLocalizedMessage}) parsing BED annotation file line: ${newLine.mkString("\t")}")
            soFar
        end try
    }
  end foldGeneFile

  /**
   * Get lists for exon starts/ends.
   * @param exonStartOffsets offsets from block start to exons
   * @param exonSizes sizes of exons
   * @param numExons # of exons
   * @param blockStart block start
   * @return (exonStarts, exonEnds)
   */
  private def getExonStartsAndEnds(exonStartOffsets: Array[Int], exonSizes: Array[Int], numExons: Int, blockStart: Int)
  : (List[Int], List[Int]) =
    // Get maximum size we can use (all sizes should be equal, but using min just in case)
    val exonsToProcess = Math.min(numExons, Math.min(exonSizes.size, exonStartOffsets.size))
    // Go through arrays and map them to lists of exon starts and ends
    exonStartOffsets.slice(0, exonsToProcess).indices.foldLeft((List.empty[Int], List.empty[Int])) {
      case ((starts, ends), index) =>
        // Get starting address of exon
        val exonStart = blockStart + exonStartOffsets(index)
        // Set exonstart and end into lists
        (exonStart +: starts, (exonStart + exonSizes(index)) +: ends)
    }
  end getExonStartsAndEnds
}
