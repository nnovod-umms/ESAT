package edu.umms.ESAT.utils

import edu.umms.ESAT.utils.Types._

import java.io.{BufferedReader, File, FileReader}
import scala.annotation.tailrec
import scala.io.Source

/**
 * Methods to read in files with a header line.
 */
object ReaderWithHeader {
  /**
   * Parse a file with a header tab delimited line.
   * @param file input file
   * @param headersNeeded headers that must be there
   * @param sep separator to use to split lines into fields
   * @param doFold callback fold in each line (resultSoFar, nextLineFieldsArray, Map of HeaderName->ArrayIndex) => T
   * @tparam T output result type for fold
   * @return folding result, otherwise error
   */
  def foldFile[T]
  (
    file: File,
    headersNeeded: Array[String],
    init: T,
    sep: String = "\t"
  )(
    doFold: (T, Array[String], Map[String, Int]) => T,
  ): T | ErrorStr =
    Reader.processFile(file) {
      (reader) =>
        // Look for header
        reader.nextOption() match
          case None =>
            error(s"No header found in ${file.getCanonicalPath}")
          case Some(header) =>
            // Get headers and make it into map of (headerName->index)
            val headers = header.split(sep)
            val headersMap = headers.zipWithIndex.toMap
            // Make sure all wanted headers are there
            headersNeeded.find(wantedHeader => !headersMap.exists(_._1 == wantedHeader)) match
              // Invalid header
              case Some(headerMissing) =>
                error(s"${file.getCanonicalPath} missing mandatory header $headerMissing")
              // Go fold together remaining lines
              case None =>
                Reader.foldFileLines(reader, init)(_.split(sep))(doFold(_, _, headersMap))
            end match
        end match
    }
  end foldFile
}
