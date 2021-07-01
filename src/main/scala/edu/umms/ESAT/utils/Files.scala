package edu.umms.ESAT.utils

import edu.umms.ESAT.utils.Types._

import java.io.File

object Files {
  /**
   * Check that we can read a file.
   * @param file file specification of file to be read
   * @return None if ok, otherwise Some(errorMessage)
   */
  def canRead(file: String): Option[ErrorStr] =
    try {
      canRead(File(file))
    } catch {
      case e =>
        Some(error(s"Error accessing file $file: ${e.getLocalizedMessage}"))
    }

  /**
   * Check that we can read a file.
   * @param file file to be read
   * @return None if ok, otherwise Some(errorMessage)
   */
  def canRead(file: File): Option[ErrorStr] = {
    // Get path for error messages
    val path = file.getCanonicalPath
    if (!file.exists())
      Some(error(s"$path not found"))
    else if (!file.isFile)
      Some(error(s"$path not a file"))
    else if (file.canRead)
      None
    else
      Some(error(s"$path not accessible"))
  }


}
