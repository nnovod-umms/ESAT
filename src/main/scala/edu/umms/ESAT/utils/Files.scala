package edu.umms.ESAT.utils

import edu.umms.ESAT.utils.Types._

import scala.reflect.Selectable.reflectiveSelectable

import java.io.File

object Files {
  /**
   * Check that we can read a file.
   * @param file file specification of file to be read
   * @return None if ok, otherwise Some(errorMessage)
   */
  def canRead(file: String): Option[ErrorStr] =
    try
      canRead(File(file))
    catch
      case e =>
        Some(error(s"Error accessing file $file: ${e.getLocalizedMessage}"))
    end try
  end canRead

  /**
   * Check that we can read a file.
   * @param file file to be read
   * @return None if ok, otherwise Some(errorMessage)
   */
  def canRead(file: File): Option[ErrorStr] =
    // Get path for error messages
    val path = file.getCanonicalPath
    if (!file.exists())
      Some(error(s"$path not found"))
    else if (!file.isFile)
      Some(error(s"$path not a file"))
    else if (!file.canRead)
      Some(error(s"$path not accessible"))
    else
      None      
    end if
  end canRead

  /**
   * Use a resourse, that has a close method, and make sure close is called after executing a specified function
   * for the resource.  Reflection is ued to make sure the resource has a close method which is called
   * after all is done.
   * @param resource resource (e.g., a file) to be used
   * @param f function to call with resource as input
   * @tparam I type for resource input to callback function
   * @tparam O output of callback function
   * @return output of callback
   */
  def using[I <: { def close(): Unit }, O](resource: I)(f: I => O): O =
    try
      f(resource)
    finally
      resource.close()
    end try
  end using
}
