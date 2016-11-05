/*
An ammonite script to extract morphological data from a 2-column tab-delimited file named as a command-line parameter.
The first column should give a URN for the passage.
The second column should be the XML text of a morphological
analysis from the Perseus morphology service.
(The output of the script parseTextBy10s.sc generates output
structured this way.)

The output is a series of three-column entries:
1. the surface form
2. the identifier for the lexical entity
3. a label for the part of speech

Usage:

    amm extractMorpheusPoS.sc FILENAME

*/

import scala.xml._
import scala.io.Source


/** Simple model of morphological identification as
* a unique identifier and a part of speech label. */
case class MorphId(lexent: String, pos: String)

// Get text content of first node in a sequence.
def textForFirstEntry (nseq: NodeSeq): String = {
  if (nseq.size > 0) {
    nseq(0).text
  } else ""
}


// create a MorphId object for each entry
def formatEntry(e: Elem) = {
  val uriGroup = e \ "@uri"
  val uri = textForFirstEntry(uriGroup).replaceFirst("http://data.perseus.org/collections/urn:cite:perseus:grclexent.","")
  val headWordList = e \\ "hdwd"
  val headWord = textForFirstEntry(headWordList)
  val posList = e \\ "pofs"
  val pos = textForFirstEntry(posList)
  MorphId(uri + "_" + headWord  ,  pos)
}

@main
def extractPoS(fName : String) {
  val parseTsv = scala.io.Source.fromFile(fName).getLines.toVector

  // an array of pairings of surface forms to
  // xml reply to morphological analysis. See
  // notes at top of script.
  val filteredArray = parseTsv.map(s => s.split("\t")).filter(_.size == 2)

  // easier to tear the pairs apart and
  // zip our results up with the id column later.
  val idColumn = filteredArray.map(_(0))
  val xmlColumn = filteredArray.map(_(1))

  // let the formatEntry column do the work
  // of creating a MorphId from an entry node
  val morphAnalyses = xmlColumn.map { e =>
    val root = XML.loadString(e)
    val entries = root \\ "entry"
    val entryData = entries.map( e => e match {
      case el: Elem => formatEntry(el)
      case _ => ""
    })
    entryData
  }

  // Zip together surface forms and analyses:
  val idAnalyzed = idColumn.zip(morphAnalyses)

  // ... and print 3-column output to standard output:
  for (wd <- idAnalyzed) {
    val analysisList = wd._2
    for (a <- analysisList) {
      a match {
        case mid: MorphId =>println(wd._1 + "\t" + mid.pos + "\t" + mid.lexent)
        case _ => println("Reply for " + wd._1 + " is broken.")
      }
    }
  }
}
