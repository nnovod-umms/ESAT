package edu.umms.ESAT.Utils

import scala.language.reflectiveCalls

object Control {
  /**
   * Use a recourse, that has a close method, and make sure close is called when we're done.  We use reflection
   * to make sure the resource has a close method and then call that close method after all is done.
   * @param resource resource (e.g., a file) to be used
   * @param f function to call with resource as input
   * @tparam I input to callback function
   * @tparam O output of callback function
   * @return output of callback
   */
  def using[I <: { def close(): Unit }, O](resource: I)(f: I => O): O =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
