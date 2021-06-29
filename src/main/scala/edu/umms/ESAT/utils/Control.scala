package edu.umms.ESAT.utils

import scala.reflect.Selectable.reflectiveSelectable

object Control {
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
    try {
      f(resource)
    } finally {
      resource.close()
    }
}