package edu.umms.ESAT.bio

import Gene._
import org.apache.log4j.{LogManager, Logger}

import scala.collection.immutable.SortedSet
import scala.collection.JavaConverters._
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Gene class.
 * @param chr chromosome
 * @param start starting location within chromosome
 * @param end ending location within chromosome
 * @param name transcription name
 * @param orientation orientation (+ or -)
 * @param exons exons found within gene
 * @param cdsStart starting location for coding sequence
 * @param cdsEnd ending location for coding sequence
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
  cdsStart: Int,
  cdsEnd: Int,
  isoForms: SortedSet[Gene] = SortedSet.empty[Gene] // IsoForms Needed?
) {
  // Make sure coding region is within gene area
  if (cdsStart != 0 && cdsStart < start)
    logger.error(s"CDS start for $chr,$name,$orientation starts before gene start ($cdsStart vs. $start)")
  end if
  if (end != 0 && end < cdsEnd)
    logger.error(s"CDS end for $chr,$name,$orientation ends before gene end ($cdsEnd vs. $end)")
  end if

  /**
   * Merge together two exon sets.  Combine them into one set and then combine overlapping exons within the set.
   * It is assumed that the exon sets are for the same chromosome and orientation.
   * @param other other Gene to get exons from
   * @return (minimumExonStart, maximumExonEnd, merged set of exons)
   */
  def mergeExons(other: Gene): (Int, Int, SortedSet[(Int, Int)]) =
    if (chr != other.chr || orientation != other.orientation)
      logger.error(s"Request to merge incompatible exons ($chr/$orientation vs. ${other.chr}/${other.orientation}")
      (start, end, exons)
    else
      // Get merged set
      val mergedSet = exons ++ other.exons
      // Return with overlapping exons folded together
      foldExons(mergedSet)
    end if
  end mergeExons
}

/**
 * Companion object
 */
object Gene {
  // Get logger
  lazy private val logger: Logger = LogManager.getLogger(this.getClass.getName)

  /**
   * Fold a set of exons to combine overlapping exons into a single exon.
   * @param exons input set of exons
   * @return (minExonStart, maxExonEnd, foldedSetOfExons)
   */
  private def foldExons(exons: SortedSet[(Int, Int)]) =
    // Fold set to get min/max location and merge exons
    val (minLoc, maxLoc, mergedList) =
      exons.toList.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE, ListBuffer.empty[(Int, Int)])) {
        case ((minSoFar, maxSoFar, listBuf), next) =>
          // Combine next exon found with those found so far.  Exons are sorted so to find an overlap just look
          // if start of new one come before end of last one
          val exonList =
            listBuf.lastOption match {
              // List has contents - look at last entry to see if new one overlaps
              case Some(last) =>
                if (next._1 > last._2)
                  // No overlap
                  listBuf.addOne(next)
                else
                  // Overlap - modify last one to include new one
                  listBuf.update(listBuf.length - 1, (last._1, Integer.max(last._2, next._2)))
                end if
                listBuf
              // List empty - set first entry
              case None =>
                listBuf.addOne(next)
            }
          // Update min/max location found and exon list
          (Integer.min(minSoFar, next._1), Integer.max(maxSoFar, next._2), exonList)
      }
    (minLoc, maxLoc, exonListToSortedSet(mergedList.toList))
  end foldExons

  /**
   * Constructor with lists of exon starts/ends.  exonStarts/exonEnds are converted into an exon set and then the normal
   * constructor is called.
   * @param chr chromosome
   * @param start starting location within chromosome
   * @param end ending location within chromosome
   * @param name transcription name
   * @param orientation orientation (+ or -)
   * @param exonStarts list of exon starts (must have same lenth as exonEnds)
   * @param exonEnds list of exon ends (must have same length as exonEnds)
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
           ): Gene =
    // Get exons into one list of (start, end)
    val exons =
      if (exonStarts.length != exonEnds.length)
        logger.warn(s"Differing number of exon starts and ends (starts: $exonStarts; ends: $exonEnds)")
        val min = Integer.min(exonStarts.length, exonEnds.length)
        exonStarts.take(min).zip(exonEnds.take(min))
      else
        exonStarts.zip(exonEnds)
      end if

    // Get minimun start and maximun end for all exons and fold overlapping exons together
    // @TODO Does it ever make sense for CDS start/end to be different from exons start/end??
    val (exonsStart, exonsEnd, geneExons) =
      if (exons.isEmpty)
        (0, 0, SortedSet.empty[(Int, Int)])
      else
        foldExons(exonListToSortedSet(exons))
      end if

    // Make new Gene object
    Gene(
      chr = chr,
      start = start, end = end,
      name = name, orientation = orientation,
      exons = geneExons,
      cdsStart = exonsStart, cdsEnd = exonsEnd
    )
  end apply

  /**
   * Convert a list of exons into a sorted set
   * @param exons exon list
   * @return exon set
   */
  private def exonListToSortedSet(exons: List[(Int, Int)]) = SortedSet(exons:_*)

  /**
   * Ordering interface for Genes
   * @return ordering functions
   */
  implicit def ordering: Ordering[Gene] = new Ordering[Gene] {
    /**
     * Compare two genes.  First compare their locations (chrmosome, start, end, orientation) and then their exons.
     * @tparam A type we're comparing (must be derived from Gene)
     * @return -1 (x < y), 0 (x = y), 1 (x > y)
     */
    override def compare(x: Gene, y: Gene): Int =
      val locC = compareLoci(x, y)
      if (locC != 0)
        locC
      else
        val exonSizeC = x.exons.size.compareTo(y.exons.size)
        if (exonSizeC != 0)
          exonSizeC
        else
          compareExons(x.exons, y.exons)
        end if
      end if
    end compare
  }

  /**
   * Compare the locations of two genes.
   * @param x first gene
   * @param y second gene
   * @return -1 (x<y) 0 (x=y) or 1 (x>y)
   */
  @inline
  private def compareLoci(x: Gene, y: Gene): Int =
    val chrC = x.chr.compareTo(y.chr)
    if (chrC != 0)
      chrC
    else
      val startC = x.start.compareTo(y.start)
      if (startC != 0)
        startC
      else
        val endC = x.end.compareTo(y.end)
        if (endC != 0)
          endC
        else
          x.orientation.compareTo(y.orientation)
        end if
      end if
    end if
  end compareLoci

  /**
   * Compare two sets of exons.  If head of sets are not equal then we exit with comparison, otherwise we recurse
   * to look at rest of sets.  If we reach the ends of both sets then the exons are considered equal.
   * @param x first exon set
   * @param y second exon set
   * @return -1 (x < y), 0 (x = y), 1 (x > y)
   */
  @tailrec @inline
  private def compareExons(x: SortedSet[(Int, Int)], y: SortedSet[(Int, Int)]): Int =
    (x.headOption, y.headOption) match
      case (None, None) => 0
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (Some(xNext), Some(yNext)) =>
        val startC = xNext._1.compareTo(yNext._1)
        if (startC != 0)
          startC
        else
          val endC = xNext._2.compareTo(yNext._2)
          if (endC != 0)
            endC
          else
            compareExons(x.tail, y.tail)
          end if
        end if
    end match
  end compareExons

  /**
   * Construct old Gene class.  Same as Java Gene constructor...
   * public Gene(String chr, int start, int end, String name, String orientation, List<Integer> exonsStart, List<Integer> exonsEnd)
   * @param chr chromosome
   * @param start starting location within chromosome
   * @param end ending location within chromosome
   * @param name transcription name
   * @param orientation orientation (+ or -)
   * @param exonStarts list of exon starts (must match up with exonEnds)
   * @param exonEnds list of exon ends (must match up with exonStarts)
   * @return java Gene class instance
   */
  def makeJavaGene(chr: String,
                   start: Int,
                   end: Int,
                   name: String,
                   orientation: String,
                   exonsStart: List[Int],
                   exonsEnd: List[Int]
                  ): umms.core.annotation.Gene =
    // Little helper method to do awkward conversion of scala List[Int] to java List[Integer]
    def getJavaIntList(list: List[Int]) = list.map(java.lang.Integer.valueOf).asJava
    // Create java Gene instance
    new umms.core.annotation.Gene(
      chr, start, end, name, orientation, getJavaIntList(exonsStart), getJavaIntList(exonsEnd)
    )
  end makeJavaGene
}
