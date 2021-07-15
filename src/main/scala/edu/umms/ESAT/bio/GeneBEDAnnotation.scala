package edu.umms.ESAT.bio

import edu.umms.ESAT.utils.Reader
import edu.umms.ESAT.utils.Types.ErrorStr
import org.apache.log4j.{LogManager, Logger}

import java.io.File

object GeneBEDAnnotation {
  // Get logger
  lazy private val logger: Logger = LogManager.getLogger(this.getClass.getName)
  // Make symbols for indicies to pieces of input line - fields must be in order they will appear in lines in file
  private val (
    chrom: Int, txStart: Int, txEnd: Int,
    geneSymbol: Int, bedScore: Int,
    orientation: Int, cdsStart: Int, cdsEnd: Int,
    pslString: Int, numExons: Int,
    exonSizes: Int, exonStarts: Int
    ) = Tuple.fromArray((0 to 11).toArray)

  /**
   * Fold BED annotation file into chosen object.
   * @param file BED annotation file
   * @param init initial value for fold
   * @param doFold callback to do fold of each line (foldedValueSoFar, chr, geneName, geneObject) => newFoldedValue
   * @tparam T type of folded value
   * @return final folded value
   */
  def foldAnnotation[T](file: File, init: T)(doFold: (T, String, String, Gene) => T): T | ErrorStr =
  // Go read in file to make map of genes found
    Reader.foldFile(file = file, init = init)(parseLine = _.split("\\s++")) {
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
          getField(fieldIndex).map(_.replaceAll("\"", "").split(",").map(_.toInt))

        try
          // Get each field, continuing so long as fields retrieved (flatMap method only called if Some(val) is returned)
          getField(chrom).flatMap(chr =>
            getField(geneSymbol).flatMap(geneName =>
              getField(txStart).flatMap(txStartStr =>
                getField(txEnd).flatMap(txEndStr =>
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
              logger.error(s"Missing fields in BED annotation file line: $newLine")
              soFar
          end match
        catch
          case e =>
            logger.error(s"Error (${e.getLocalizedMessage}) parsing BED annotation file line: $newLine")
            soFar
        end try
    }
  end foldAnnotation

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
    // Helper to make an integer from a string
    // @TODO replaceAll needed?  Was in setBlockStartsAndEnds of old Gene code
    def makeInt(s: String) = Integer.parseInt(s.replaceAll("\"", "").trim)

    // Get maximum size we can use
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
