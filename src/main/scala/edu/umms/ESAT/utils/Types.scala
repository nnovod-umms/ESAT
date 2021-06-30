package edu.umms.ESAT.utils

object Types {
  // Type for errors
  opaque type ErrorStr = String

  /**
   * Convert string to error
   * @param err error message
   * @return Error instance of message
   */
  @inline
  def error(err: String): ErrorStr = err.asInstanceOf[ErrorStr]
}
