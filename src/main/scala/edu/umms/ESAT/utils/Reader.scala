package edu.umms.ESAT.utils

import edu.umms.ESAT.utils.Control
import edu.umms.ESAT.utils.Types._

import java.io.{BufferedReader, File, FileReader}
import scala.annotation.tailrec
import scala.io.Source

object Reader {
  /**
   * Process a file.  Any errors are caught and reported as an ErrorStr.
   * @param file file to process
   * @param processor method to process file, called with iterator to get lines from file
   * @tparam T processor output
   * @return output from processor or error string
   */
  def processFile[T](file: File)(processor: (Iterator[String] => T)): T | ErrorStr = {
    try {
      // Open file using controller that guarantees file will be closed
      Control.using(Source.fromFile(file)) {
        input =>
          // Get iterator and go process file
          val fileLines = input.getLines()
          processor(fileLines)
      }
    } catch {
      case e =>
        error(s"Error processing input from file ${file.getCanonicalPath}: ${e.getLocalizedMessage}")
    }
  }

  /**
   * Parse a file with delimited lines, folding lines into a single output.
   * @param file input file
   * @param init initial value for doing fold
   * @param sep separator to use to split lines
   * @param doFold callback fold in each line (resultSoFar, nextLineFieldsArray, Map of HeaderName->ArrayIndex) => T
   * @tparam T output result type for fold
   * @return folding result, otherwise error
   */
  def foldFile[T]
  (
    file: File,
    init: T,
    sep: String
  )(
    doFold: (T, Array[String]) => T,
  ): T | ErrorStr = {
    processFile(file) {
      (reader) =>
        reader.foldLeft(init) {
          case (soFar, next) => doFold(soFar, next.split(sep))
        }
    }
  }
}
