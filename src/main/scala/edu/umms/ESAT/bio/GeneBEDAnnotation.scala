package edu.umms.ESAT.bio

import edu.umms.ESAT.utils.Reader
import edu.umms.ESAT.utils.Types.ErrorStr
import org.apache.log4j.{LogManager, Logger}

import java.io.File

object GeneBEDAnnotation {
  // Get logger
  lazy private val logger: Logger = LogManager.getLogger(this.getClass.getName)
  // Make symbols for indicies to pieces of input line - must be in order fields will appear in lines in file
  private val (
    chrom: Int, txStart: Int, txEnd: Int,
    geneSymbol: Int, bedScore: Int,
    orientation: Int, cdsStart: Int, cdsEnd: Int,
    exonStarts: Int, exonEnds: Int
    ) = Tuple.fromArray((0 to 9).toArray)

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
        def getIntList(fieldIndex: Int): Option[List[Int]] =
          getField(fieldIndex).map(_.split(",").map(_.toInt).toList)

        try
          // Get each field, continuing so long as fields retrieved (flatMap method only called if Some(val) is returned)
          getField(chrom).flatMap(chr =>
            getField(geneSymbol).flatMap(geneName =>
              getField(txStart).flatMap(txStartStr =>
                getField(txEnd).flatMap(txEndStr =>
                  getField(orientation).flatMap(strand =>
                    getIntList(exonStarts).flatMap(exonStartList =>
                      getIntList(exonEnds).map(exonEndList => {
                        // We retrieved all the fields needed - make a Gene and callback to fold it in
                        val newGene = Gene(
                          chr = chr, start = txStartStr.toInt, end = txEndStr.toInt,
                          name = geneName, orientation = strand,
                          exonStarts = exonStartList, exonEnds = exonEndList
                        )
                        doFold(soFar, chr, geneName, newGene)
                      })
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
}
