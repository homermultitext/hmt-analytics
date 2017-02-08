/*
Ammonite script for experimenting with
tokenizing HMT for topic modelling.

Works on a HMT tabulated file: input expected
to be one well-formed xml frag. per line.

*/
import scala.io.Source
import scala.xml._



// Two blacklists:  first, TEI elements that should
// *always* be ignored:
val ignorable = Vector("abbr", "orig","sic", "num")
// TEI elements ignored as one of a pair in a `choice`:
val ignoreInChoicePair = Vector("EMPTY")


// Extract well-formed xml frag
// from HMT internal 7-col format.
def extractText(s: String) = {
  val cols = s.split("#")
  if (cols.size > 5) {
    cols(5)
  } else {
    ""
  }
}

// Extract URN value
// from HMT internal 7-col format.
def extractUrn(s: String) = {
  val cols = s.split("#")
  cols(0)
}

// parses HMT edition in an xml.Node and creates
// a string that can be split on strings and punctuation
def preptokens (n: xml.Node, s: String): String = {
  var txt = s
  n match {
    case t: xml.Text => { txt = txt + t.text}
    case e: xml.Elem => {
      val parent = e.label
      for (ch <- e.child) {
        if (ignorable.contains(ch.label)) {
          // ignore
        } else if ((parent == "choice")  &&
        (ignoreInChoicePair.contains(ch.label)) ) {
          //ignore
        } else {
          txt += preptokens(ch, s)
        }
      }
    }
  }
  txt
}

def stringifyXml(s: String) = {
  val root  = XML.loadString(s)
  preptokens(root,"")
}

def allWhite(s: String) = {
  val whitespace = "^[ ]+$".r
  val wsmatch = whitespace.findFirstIn(s)
  wsmatch match {
    case Some(s) => true
    case None => false
  }
}

@main
def edtokens(f: String) = {
  val textLines = scala.io.Source.fromFile(f).getLines.toVector.filterNot(_.isEmpty)
  val extracts = textLines.map( s => (extractUrn(s), extractText(s) ))
  val pairs = extracts.filterNot(_._2.isEmpty)
  val stringified = pairs.map { case (urn,xml) => (urn,stringifyXml(xml)) }

  val edited = stringified.filterNot(pr => allWhite(pr._2))
  for(psg <- edited) {
    println(psg._1 + "\t" + psg._2)
  }
}
