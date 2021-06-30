package edu.umms.ESAT.utils

import edu.umms.ESAT.utils.Control
import edu.umms.ESAT.utils.Types._

import java.io.{BufferedReader, File, FileReader}
import scala.annotation.tailrec
import scala.io.Source

object ReaderWithHeader {
  /**
   * Parse a file with a header tab delimited line.
   * @param file input file
   * @param headersNeeded headers that must be there
   * @param sep separator to use to get
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
  {
    // Get path for error messages
    val path = file.getCanonicalPath
    try {
      // Go process file (using will close it)
      Control.using(Source.fromFile(file)) {
        input =>
          // Get reader to iterate over lines
          val reader = input.getLines()
          // Look for header
          reader.nextOption() match {
            case None =>
              error(s"No header found in $path")
            case Some(header) =>
              // Get headers and make it into map of (headerName->index)
              val headers = header.split(sep)
              val headersMap = headers.zipWithIndex.toMap
              // Make sure all wanted headers are there
              headersNeeded.find(wantedHeader => !headersMap.exists(_._1 == wantedHeader)) match {
                case Some(headerMissing) =>
                  error(s"$path missing mandatory header $headerMissing")
                case None =>
                  reader.foldLeft(init) {
                    case (soFar, next) => doFold(soFar, next.split(sep), headersMap)
                  }
                  /*
                   * Recursive fold to callback to fold in lines as we parse them.
                   * @param input remaining input
                   * @param soFar results of fold so far
                   * @return fold result
                   */
                  //@tailrec
                  //def fold(input: Iterator[String], soFar: T): T = {
                  //  input.nextOption() match {
                  //    case None => soFar
                  //    case Some(line) =>
                  //      val next = doFold(soFar, line.split(sep), headersMap)
                  //      fold(input, next)
                  //  }
                  //}
                  // Do fold
                  //fold(reader, init)
              }
          }
      }
    } catch {
      case e => error(s"Error processing input from file $path: ${e.getLocalizedMessage}")
    }
  }
}
