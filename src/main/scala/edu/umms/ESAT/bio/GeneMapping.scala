package edu.umms.ESAT.bio

import edu.umms.ESAT.utils.ReaderWithHeader
import edu.umms.ESAT.utils.Types.ErrorStr

import java.io.File

/**
 * Process gene mapping files
 */
object GeneMapping {
  // Mapping file headers
  private val geneMappingHeaders =
    Array("name2", "chrom", "txStart", "txEnd", "name", "strand", "exonStarts", "exonEnds")
  // Make symbols from headers
  private val (
    geneSymbol: String, chrom: String,
    txStart: String, txEnd: String, txID: String,
    strand: String, exonStarts: String, exonEnds: String
    ) = Tuple.fromArray(geneMappingHeaders)

  /**
   * Fold gene mapping file into chosen object
   * @param file gene mapping file
   * @param init initial value for fold
   * @param doFold callback to do fold of each line (foldedValueSoFar, chr, geneName, geneObject) => newFoldedValue
   * @tparam T type of folded value
   * @return final folded value
   */
  def foldMapping[T](file: File, init: T)(doFold: (T, String, String, Gene) => T): T | ErrorStr =
    // Go read in file to make map of genes found
    ReaderWithHeader.foldFile(
      file = file, headersNeeded = geneMappingHeaders, init = init, sep = "\t"
    ){(soFar, newLine, headerIndicies) =>
      // Some little helper methods
      @inline
      def getField(fieldName: String): String = newLine(headerIndicies(fieldName))
      @inline
      def getIntList(fieldName: String) = getField(fieldName).split(",").map(_.toInt).toList

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
        val newGene = Gene(
          chr = chr,
          start = getField(txStart).toInt, end = getField(txEnd).toInt,
          name = getField(txID), orientation = getField(strand),
          exonStarts = getIntList(exonStarts), exonEnds = getIntList(exonEnds)
        )
        doFold(soFar, chr, geneName, newGene)
    }
  end foldMapping
}
