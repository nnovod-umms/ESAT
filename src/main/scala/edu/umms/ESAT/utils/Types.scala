package edu.umms.ESAT.utils

object Types {
  // Type for errors
  opaque type Error = String

  /**
   * Convert string to error
   * @param err error message
   * @return Error instance of message
   */
  def Error(err: String): Error = err.asInstanceOf[Error]
}
