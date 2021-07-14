package edu.umms.ESAT.utils

import edu.umms.ESAT.utils.Files
import edu.umms.ESAT.utils.Types._

import java.io.{BufferedReader, File, FileReader}
import scala.annotation.tailrec
import scala.io.Source

/**
 * Methods to read in files.
 */
object Reader {
  /**
   * Process a file.  Any errors are caught and reported as an ErrorStr.
   * @param file file to process
   * @param processor method to process file, called with iterator to get lines from file
   * @tparam T processor output
   * @return output from processor or error string
   */
  def processFile[T](file: File)(processor: (Iterator[String] => T)): T | ErrorStr =
    try
      // Open file using controller that guarantees file will be closed
      Files.using(Source.fromFile(file)) {
        input =>
          // Get iterator and go process file
          val fileLines = input.getLines()
          processor(fileLines)
      }
    catch
      case e =>
        error(s"Error processing input from file ${file.getCanonicalPath}: ${e.getLocalizedMessage}")
    end try
  end processFile

  /**
   * Parse a file with delimited lines, folding lines into a single output.
   * @param file input file
   * @param init initial value for doing fold
   * @param sep regular expression to use to split lines
   * @param parseLine callback to do initial parse of a line of input (inputLine) => parsedLine
   * @param doFold callback fold in each line (resultSoFar, nextParsedLine, Map of HeaderName->ArrayIndex) => T
   * @tparam L output of parsing line and input to doFold
   * @tparam T output result type for fold
   * @return folding result, otherwise error
   */
  def foldFile[L, T](file: File, init: T)(parseLine: (String) => L)(doFold: (T, L) => T)
  : T | ErrorStr =
    processFile(file) {
      (reader) => foldFileLines(reader, init)(parseLine)(doFold)
    }
  end foldFile

  /**
   * Fold lines of a file into a single result.
   * @param reader file lines
   * @param init initial value to start fold
   * @param sep regular expression to use to split lines
   * @param parseLine callback to do initial parse of a line of input (inputLine) => parsedLine
   * @param doFold callback to process each line: (resultsSoFar, lineFields) => newResults
   * @tparam L output of parsing line and input to doFold
   * @tparam T results type
   * @return results of folding file lines
   */
  def foldFileLines[L, T](reader: Iterator[String], init: T)(parseLine: (String) => L)(doFold: (T, L) => T)
  : T =
    reader.foldLeft(init) {
      case (soFar, next) => doFold(soFar, parseLine(next))
    }
  end foldFileLines
}
