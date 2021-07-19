package edu.umms.ESAT.bio

import edu.umms.ESAT.utils.Types.ErrorStr

import java.io.File

/**
 * Trait containing methods to parse input files with gene data.
 */
trait GeneFileFold 
{
  /**
   * Fold file containing gene input data into chosen object.
   * @param file input gene file
   * @param init initial value for fold
   * @param doFold callback to do fold of each line (foldedValueSoFar, chr, geneName, geneObject) => newFoldedValue
   * @tparam T type of folded value
   * @return final folded value
   */
  def foldGeneFile[T](file: File, init: T)(doFold: (T, String, String, Gene) => T): T | ErrorStr
}
