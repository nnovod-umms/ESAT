package edu.umms.ESAT.parse

import edu.umms.ESAT.parse.Params.MultiMap._
import edu.umms.ESAT.parse.Params.Task._
import edu.umms.ESAT.parse.Params._

import java.io.File

case class Params
(
  task: Task,
  outFile: File,
  inFiles: Seq[File],
  inExperiment: String,
  alignments: Option[File],
  //bamFiles: Map[String, List[File]]= Map.empty, // key=experiment ID, File[]= list of input files for the experiment
  //mmBamFiles: Map[String, List[File]] = Map.empty, // key=experiment ID, File[]= list of input files for the experiment (for 'proper' multimap handling)
  annotationFile: Option[File],
  windowLength: Int,
  windowOverlap: Int,
  windowExtend: Int,
  allWindows: Boolean, // save all significant windows (default, only single window position with the highest counts
  // within a set of contiguous overlapping windows.)
  multimap: MultiMap, // one of "ignore", "normal" or "scale"
  //No longer needed - was just used to say if qThresh was present: qFilter: Boolean = false,
  qThresh: Option[Int], // quality threshold (reads must be GREATER THAN qThresh, if filtering is on
  gMapFile: Option[File], // name of the gene mapping file
  pValThresh: Float, // minimum allowable p-value for window significance testing
  stranded: Boolean, // allow for unstranded analysis (defaults to stranded)
  /* single-cell parameters */
  scPreprocess: Boolean, // inDrop library reads preprocessing flag
  // NOTE: barcode and UMIs are appended to the read name, with the format "<readName>:<barcode>:<UMI>".
  bcMin: Int, // minimum number of reads that must be observed for a barcode to be considered valid (after PCR duplicate removal)
  umiMin: Int, // minimum number times a barcode:transcript:UMI must occur for it to be considered valid
  /* optional AT filter */
  // No longer needed - was just used to say if filtAtN was present: filtAT: Boolean, // A/T filtering flag. True=filter out reads with long stretches of As of Ts
  filtAtN: Option[Int], // length of maximum length of A/T stretches. Reads with stretches of A/T >= filtAtN will be removed.
  //countsMap: Map[String, Map[String, TranscriptCountInfo]] = Map.empty,
  //bamDict: SAMSequenceCountingDict,
  //geneTable: Hashtable[String, Gene],
  //inDropData: InDropPreprocess,
  //expMap: ExperimentMap
)

object Params {
  // Default values for parameters not specified on command line
  private[parse] val taskDef = "score3p"
  private[parse] val expDef = "exp1"
  private[parse] val windowLenDef = 50
  private[parse] val windowOverlapDef = 0
  private[parse] val windowExtendDef = 0
  private[parse] val multiMapDef = "normal"
  private[parse] val pValThreshDef = 1.0f
  private[parse] val bcMinDef = 0
  private[parse] val umiMinDef = 1
  private[parse] val allWindowsDef = false
  private[parse] val strandedDef = true
  private[parse] val scPreprocessDef = false


  /**
   * Base for enums with names
   * @param name name given to enumerated value
   */
  sealed trait NamedEnum(val name: String)

  /**
   * Companion object with methods to work on named enums
   */
  object NamedEnum {
    /**
     * Get enum value that matches string.  If none found then throws exception.  Expected 
     * @param str string to match
     * @param values enum values
     * @tparam T NamedEnum type
     * @return enum value found
     */
    private[parse] def find[T <: NamedEnum](str: String, values: Array[T]): T =
      values.find(enumVal => enumVal.name == str) match {
        case Some(enumVal) => enumVal
        case _ => throw Exception("Invalid enumerated value.")
      }
  }

  /**
   * Task set to 3' or 5'
   */
  enum Task(name: String) extends NamedEnum(name) {
    case SCORE3P extends Task("score3p")
    case SCORE5P extends Task("score5p")
  }

  /**
   * Multimap read treatment
   */
  enum MultiMap(name: String) extends NamedEnum(name) {
    case NORMAL extends MultiMap("normal")
    case IGNORE extends MultiMap("ignore")
    case SCALE extends MultiMap("scale")
    case PROPER extends MultiMap("proper")
  }
}