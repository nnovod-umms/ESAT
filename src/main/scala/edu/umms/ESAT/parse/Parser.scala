package edu.umms.ESAT.parse

import edu.umms.ESAT.parse.Parser
import edu.umms.ESAT.parse.Params._
import edu.umms.ESAT.parse.Params.MultiMap._
import edu.umms.ESAT.parse.Params.Task._
import scopt.OParser

import java.io.File

object Parser {
  // scopt readers to parse value types not built in to base scopt
  // note that when readers throw exceptions they are picked up by scopt and cleanly set as parsing errors
  // scopt readers for enumerated values
  implicit val taskRead: scopt.Read[Task] =
    scopt.Read.reads(s => Task.find(s))
  implicit val multiMap: scopt.Read[MultiMap] =
    scopt.Read.reads(s => MultiMap.find(s))
  // scopt reader for Float (strange this one isn't standard)
  implicit val float: scopt.Read[Float] =
    scopt.Read.reads(s => s.toFloat)

  // Build parser options for Params
  private val builder = OParser.builder[Params]

  // Create a parser for parameters
  private val esatParser = {
    import builder._ // Bring in options for building Params parser (programName, head, opt, ...)
    // Specify parser options
    OParser.sequence(
      programName("ESAT"),
      head("ESAT", "0.1"),
      opt[Seq[File]]('i', "in")
        .valueName("<file1>,<file2>,...")
        .action((bams, c) => c.copy(inFiles = bams))
        .text("input bam file(s)")
      ,
      opt[String]('e',"experiment")
        .valueName("<string>")
        .action((exp, c) => c.copy(inExperiment = exp))
        .text(s"experiment name for input bam file(s) (default '$expDef')")
      ,
      opt[File]('a',"alignments")
        .valueName("<file>")
        .action((algn, c) => c.copy(alignments = Some(algn)))
        .text("input file containing list of <experimentName><TAB><bamFileName>")
      ,
      opt[File]('o', "out")
        .required()
        .valueName("<file>")
        .action((out, c) => c.copy(outFile = out))
        .text("output file base name")
      ,
      opt[File]('n',"annotations")
        .valueName("<file>")
        .action((ann, c) => c.copy(annotationFile = Some(ann)))
        .text("reference annotation BED file")
      ,
      opt[File]('g', "geneMapping")
        .valueName("<file>")
        .action((gm, c) => c.copy(gMapFile = Some(gm)))
        .text("gene-to-transcript map file")
      ,
      opt[Int]('q', "quality")
        .valueName("<integer>")
        .action((qual, c) => c.copy(qThresh = qual))
        .text(s"optional minimum alignment quality (default $qThreshDef)")
      ,
      opt[Task]('t', "task")
        .valueName("score3p or score5p")
        .action((t, c) => c.copy(task = t))
        .text(s"3' or 5' library (default '$taskDef')")
      ,
      opt[MultiMap]('m', "multimap")
        .valueName("normal, ignore, scale or proper")
        .action((m, c) => c.copy(multimap = m))
        .text(s"multimap read treatment (default '$multiMapDef')")
      ,
      opt[Unit]('u', "unstranded")
        .action((_, c) => c.copy(stranded = false))
        .text("alignments are unstranded (stranded by default)")
      ,
      opt[Int]('f', "filtAT")
        .valueName("<integer>")
        .action((fat, c) => c.copy(filtAtN = Some(fat)))
        .text("optional removal of reads with contiguous As or Ts (not removed by default)")
      ,
      opt[Int]('w', "wLen")
        .valueName("<integer>")
        .action((wl, c) => c.copy(windowLength = wl))
        .text(s"optional window length (default $windowLenDef)")
      ,
      opt[Int]('v', "wOverlap")
        .valueName("<integer>")
        .action((wo, c) => c.copy(windowOverlap = wo))
        .text(s"optional window overlap (default $windowOverlapDef)")
      ,
      opt[Int]('x', "wExtend")
        .valueName("<integer>")
        .action((we, c) => c.copy(windowExtend = we))
        .text(s"optional window extension past end of transcript (default $windowExtendDef)")
      ,
      opt[Unit]('l', "all")
        .action((_, c) => c.copy(allWindows = true))
        .text("save all significant windows (default is single window position with highest counts within contiguous overlapping windows.)")
      ,
      opt[Float]('s', "sigTest")
        .valueName("<float>")
        .action((st, c) => c.copy(pValThresh = st))
        .text(s"optional minimum allowable p-value for window significance testing (default $pValThreshDef)")
      ,
      opt[Unit]('p', "scPrep")
        .action((_, c) => c.copy(scPreprocess = true))
        .text("do preprocessing of single cells (not done by default)")
      ,
      opt[Int]('b', "bcMin")
        .valueName("<integer>")
        .action((bm, c) => c.copy(bcMin = bm))
        .text(s"optional minimum bar code count (default $bcMinDef)")
      ,
      opt[Int]('u', "umiMin")
        .valueName("<integer>")
        .action((um, c) => c.copy(umiMin = um))
        .text(s"optional minimum umi count (default $umiMinDef)")
      ,
      help('h', "help").text("prints this usage text"),
      note("\nNotes:\nEither, but not both, --in or --alignments must be specified\n"+
        "Either, but not both, --geneMapping or --annotation must be specified"),
      checkConfig(
        c =>
          if (c.alignments.isDefined && c.inFiles.nonEmpty)
            failure("can not specify both --in and --alignments")
          else if (c.alignments.isEmpty && c.inFiles.isEmpty)
            failure("must specifiy either --in or --alignments")
          else if (c.annotationFile.isDefined && c.gMapFile.isDefined)
            failure("can not specify both --annotations and --geneMapping")
          else if (c.annotationFile.isEmpty && c.gMapFile.isEmpty)
            failure("must specifiy either --annotations or --geneMapping")
          else if (c.qThresh < 0)
            failure(s"--quality (${c.qThresh}) must be >= 0")
          else if (c.windowLength < 1)
            failure(s"--wLen (${c.windowLength}) must be >= 1")
          else if (c.windowOverlap < 0)
            failure(s"--wOverlap (${c.windowOverlap}) must be >= 0")
          else if (c.windowExtend < 0)
            failure(s"--wExtend (${c.windowExtend}) must be >= 0")
          else if (c.pValThresh == Float.NaN)
            failure("Invalid floating point number specified for --sigTest")
          else
            success
      )
    )
  }

  /**
   * Parse input arguments.  Arguments are validated and if any errors are found they are output to stderr and
   * no parameters are returned.  If all goes well parameters parsed are set in the output.
   * @param args input arguments
   * @return parameters filled in, or None if errors found in input.
   */
  def doParse(args: Array[String]): Option[Params] = {
    // Initial parameter setup with defaults to be passed into parser
    val initParams = Params(
      task = Task.find(taskDef),
      outFile = File("ToBeReplaced"),
      inFiles = Seq.empty,
      inExperiment = expDef,
      alignments = None,
      annotationFile = None,
      windowLength = windowLenDef,
      windowOverlap = windowOverlapDef,
      windowExtend = windowExtendDef,
      allWindows = allWindowsDef,
      multimap = MultiMap.find(multiMapDef),
      qThresh = qThreshDef,
      gMapFile = None,
      pValThresh = pValThreshDef,
      stranded = strandedDef,
      scPreprocess = scPreprocessDef,
      bcMin = bcMinDef,
      umiMin = umiMinDef,
      filtAtN = None
    )
    OParser.parse(esatParser, args, initParams)
  }
}

