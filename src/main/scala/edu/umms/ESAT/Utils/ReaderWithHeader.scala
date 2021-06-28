package edu.umms.ESAT.Utils

import edu.umms.ESAT.Utils.Control

import java.io.{BufferedReader, File, FileReader}
import scala.io.Source

object ReaderWithHeader {
  /**
   * Parse a file with a header tab delimited line.
   * @param file input file
   * @param headersNeeded headers that must be there
   * @param nextLine callback to get each line (arrayOfInput, Map of HeaderName->ArrayIndex) => T
   * @tparam T
   * @return None if all went well, otherwise Some(error)
   */
  def parseFile[T](sep: String = "\t")
  (
    file: File, headersNeeded: List[String],
    nextLine: (Array[String], Map[String, Int]) => T
  ): Option[String] = {
    Control.using(Source.fromFile(file)) {
      input =>
        val reader = input.getLines()
        reader.nextOption() match {
          case None =>
            Some(s"No header found in ${file.getCanonicalPath}")
          case Some(header) =>
            val headers = header.split(sep)
            val headersMap = headers.zipWithIndex.toMap
            headersNeeded.find(wantedHeader => !headersMap.exists(_._1 == wantedHeader)) match {
              case Some(headerMissing) =>
                Some(s"${file.getCanonicalPath} missing mandatory header $headerMissing")
              case None =>
                try {
                  for (line <- reader) {
                    nextLine(line.split(sep), headersMap)
                  }
                  None
                } catch {
                  case e => Some(s"Error processing input: ${e.getLocalizedMessage}")
                }
            }
        }
    }
  }
}
