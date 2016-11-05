import scala.xml._
import scala.io.Source



def textForFirstEntry (nseq: NodeSeq): String = {

  if (nseq.size > 0) {
    nseq(0).text
  } else ""
}

def idForEntry (nseq: NodeSeq): String = {

  if (nseq.size > 0) {
      nseq(0).text.replaceFirst("http://data.perseus.org/collections/urn:cite:perseus:grclexent.","")
  } else ""
}


// create a tuple of id + pos for each entry
def formatEntry(e: Elem) = {
  val uriGroup = e \ "@uri"
  val uri = idForEntry(uriGroup)
  val headWordList = e \\ "hdwd"
  val headWord = textForFirstEntry(headWordList)
  val posList = e \\ "pofs"
  val pos = textForFirstEntry(posList)
  (uri + "_" + headWord  ,  pos)
}

@main
def extractPoS(fName : String) {

  val parseTsv = scala.io.Source.fromFile(fName).getLines.toVector
  val filteredArray = parseTsv.map(s => s.split("\t")).filter(_.size == 2)
  println("Work on " + filteredArray.size + " entries.")
  val entryCount = filteredArray.map { e =>
    val root = XML.loadString(e(1))
    val entries = root \\ "entry"
    val entryData = entries.map( e => e match {
      case el: Elem => formatEntry(el)
      case _ => ""

    })
    println(entryData)
  }
}
