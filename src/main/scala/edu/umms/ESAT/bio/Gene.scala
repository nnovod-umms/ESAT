package edu.umms.ESAT.bio

import scala.collection.immutable.SortedSet
import scala.collection.JavaConverters._
import Gene._
import org.apache.log4j.{LogManager, Logger}

import scala.collection.mutable.ListBuffer

/**
 * Gene class.
 * @param chr chromosome
 * @param start starting location within chromosome
 * @param end ending location within chromosome
 * @param name transcription name
 * @param orientation orientation (+ or -)
 * @param exons exons found
 * @param isoForms other genes merged into this one
 */
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
   * Merge together two exon sets.  Combine them into one set and then combine overlapping exons within the set.
   * It is assumed that the exon sets are for the same chromosome and orientation.
   * @param other other Gene to get exons from
   * @return (minimumExonStart, maximumExonEnd, merged set of exons)
   */
  def mergeExons(other: Gene): (Int, Int, SortedSet[(Int, Int)]) = {
    if (chr != other.chr || orientation != other.orientation) {
      logger.error(s"Request to merge incompatible exons ($chr/$orientation vs. ${other.chr}/${other.orientation}")
      (start, end, exons)
    } else {
      // Get merged set
      val mergedSet = exons ++ other.exons
      // Fold set to get min/max location and merge exons
      val (minLoc, maxLoc, mergedList) =
        mergedSet.toList.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE, ListBuffer.empty[(Int, Int)])) {
          case ((minSoFar, maxSoFar, listBuf), next) =>
            // Combine next exon found with those found so far.  Exons are sorted so to find an overlap just look
            // if start of new one come before end of last one
            val exonList =
              listBuf.lastOption match {
                // List has contents - look at last entry to see if new one overlaps
                case Some(last) =>
                  if (next._1 > last._2) {
                    // No overlap
                    listBuf.addOne(next)
                  } else {
                    // Overlap - modify last one to include new one
                    listBuf.update(listBuf.length - 1, (last._1, Integer.max(last._2, next._2)))
                  }
                  listBuf
                // List empty - set first entry
                case None =>
                  listBuf.addOne(next)
              }
            // Update min/max location found and exon list
            (Integer.min(minSoFar, next._1), Integer.max(maxSoFar, next._2), exonList)
        }
      (minLoc, maxLoc, exonListToSortedSet(mergedList.toList))
    }
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

/**
 * Companion object
 */
object Gene {
  // Get logger
  lazy private val logger: Logger = LogManager.getLogger(this.getClass.getName)

  /**
   * Convert a list of exons into a sorted set
   * @param exons exon list
   * @return exon set
   */
  private def exonListToSortedSet(exons: List[(Int, Int)]) = SortedSet(exons:_*)

  /**
   * Constructor with lists of exon starts/ends.  Starts/ends are converted into an exon set and then the normal
   * constructor is called.
   * @param chr chromosome
   * @param start starting location within chromosome
   * @param end ending location within chromosome
   * @param name transcription name
   * @param orientation orientation (+ or -)
   * @param exonStarts list of exon starts (must match up with exonEnds)
   * @param exonEnds list of exon ends (must match up with exonStarts)
   * @return Gene with input settings
   */
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
      exons = exonListToSortedSet(exons)
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
      else {
        // @TODO - compare exons?  What about CDS region (see old Gene/compareTo in umms.core.annotation.Gene)
        locC
      }
    }
  }

  // 	public Gene(String chr, int start, int end, String name, String orientation, List<Integer> exonsStart, List<Integer> exonsEnd)

  /**
   * Construct old Gene class.
   * @param chr chromosome
   * @param start starting location within chromosome
   * @param end ending location within chromosome
   * @param name transcription name
   * @param orientation orientation (+ or -)
   * @param exonStarts list of exon starts (must match up with exonEnds)
   * @param exonEnds list of exon ends (must match up with exonStarts)
   * @return java Gene class
   */
  def makeJavaGene(
    chr: String,
    start: Int,
    end: Int,
    name: String,
    orientation: String,
    exonsStart: List[Int],
    exonsEnd: List[Int]
  ): umms.core.annotation.Gene = {
    // Little helper method to do awkward conversion of scala List[Int] to java List[Integer]
    def getJavaIntList(list: List[Int]) = list.map(java.lang.Integer.valueOf).asJava
    
    new umms.core.annotation.Gene(
      chr, start, end, name, orientation, getJavaIntList(exonsStart), getJavaIntList(exonsEnd)
    )
  }

}
