package edu.umms.ESAT.bio

import scala.collection.immutable.SortedSet
import scala.collection.JavaConverters._
import Gene._
import org.apache.log4j.{LogManager, Logger}

import scala.collection.mutable.ListBuffer

case class Gene
(
  chr: String,
  start: Int,
  end: Int,
  name: String,
  orientation: String,
  exons: SortedSet[(Int, Int)],
  isoForms: SortedSet[Gene] = SortedSet.empty[Gene] // Needed?
) {
  /**
   * Merge together two exon sets.  We combine them into one set and then combine overlapping ones within the set.
   * It is assume that the exon sets are for the same chromosome and orientation.
   * @param other
   * @return (minimumExonStart, maximumExonEnd, merged set of exons)
   */
  def mergeExons(other: Gene): (Int, Int, SortedSet[(Int, Int)]) = {
    val mergedSet = exons ++ other.exons
    val (minLoc, maxLoc, mergedList) =
      mergedSet.toList.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE, ListBuffer.empty[(Int, Int)])) {
        case ((minSoFar, maxSoFar, listBuf), next) =>
          val exonList =
            listBuf.lastOption match {
              case Some(last) =>
                if (next._1 > last._2)
                  listBuf.addOne(next)
                else {
                  listBuf.update(listBuf.length - 1, (last._1, Integer.max(last._2, next._2)))
                }
                listBuf
              case None =>
                listBuf.addOne(next)
            }
          (Integer.min(minSoFar, next._1), Integer.max(maxSoFar, next._2), exonList)
      }
    (minLoc, maxLoc, exonListToSortedSet(mergedList.toList))
  }

  /*
  private def sortExons(exons: Array[(Int, Int)]): Array[(Int, Int)] =
    exons.sortWith {
      case ((start1, end1), (start2, end2)) =>
        start1 < end1 || ((start1 == end1) && start2 < end2)
    }

  def mergeExons(exons2: Array[(Int, Int)]) = {
    sortExons((exons ++ exons2)).foldLeft(ListBuffer.empty[(Int, Int)]) {
      case (listBuf, next) =>
        listBuf.lastOption match {
          case Some(last) =>
            if (next._1 > last._2)
              listBuf.addOne(next)
            else {
              listBuf.update(listBuf.length - 1, (last._1, Integer.max(last._2, next._2)))
            }
            listBuf
          case None =>
            listBuf.addOne(next)
        }
    }
  }
  */
}

object Gene {
  // Get logger
  lazy val logger: Logger = LogManager.getLogger(this.getClass.getName)

  private def exonListToSortedSet(exons: List[(Int, Int)]) = SortedSet(exons:_*)

  def apply(
             chr: String,
             start: Int,
             end: Int,
             name: String,
             orientation: String,
             exonStarts: List[Int],
             exonEnds: List[Int]
           ): Gene = {
    val exons =
      if (exonStarts.length != exonEnds.length) {
        logger.warn(s"Differing number of exon starts and ends (starts: $exonStarts; ends: $exonEnds)")
        val min = Integer.min(exonStarts.length, exonEnds.length)
        exonStarts.take(min).zip(exonEnds.take(min))
      } else
        exonStarts.zip(exonEnds)
    // Make new Gene object
    Gene(
      chr = chr,
      start = start, end = end,
      name = name, orientation = orientation,
      exons = collection.immutable.SortedSet(exons:_*)
    )
  }

  //@TODO need better comparison?
  @inline
  private def compareLoci(x: Gene, y: Gene): Int =
    val chrC = x.chr.compareTo(y.chr)
    if (chrC != 0)
      chrC
    else {
      val startC = x.start.compareTo(y.start)
      if (startC != 0)
        startC
      else {
        val endC = x.end.compareTo(y.end)
        if (endC != 0)
          endC
        else
          x.orientation.compareTo(y.orientation)
      }
    }

  implicit def ordering[A <: Gene]: Ordering[Gene] = new Ordering[Gene] {
    override def compare(x: Gene, y: Gene): Int = {
      val locC = compareLoci(x, y)
      if (locC != 0)
        locC
      else
        locC
    }
  }

  // 	public Gene(String chr, int start, int end, String name, String orientation, List<Integer> exonsStart, List<Integer> exonsEnd)
  def makeJavaGene(
    chr: String,
    start: Int,
    end: Int,
    name: String,
    orientation: String,
    exonsStart: List[Int],
    exonsEnd: List[Int]
  ): umms.core.annotation.Gene = {
    def getJavaIntList(list: List[Int]) = list.map(java.lang.Integer.valueOf).asJava
    new umms.core.annotation.Gene(
      chr, start, end, name, orientation,
      getJavaIntList(exonsStart), getJavaIntList(exonsEnd)
    )
  }

}
