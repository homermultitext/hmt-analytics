/*
Ammonite script for experimenting with HMT
editorial tokenizing.

Should work on tabulated file: input expected
to be one well-formed xml frag. per line.

*/
import scala.io.Source
import scala.xml._



// Two blacklists:  first, TEI elements that should
// *always* be ignored:
val ignorable = Vector("abbr", "orig")
// TEI elements ignored as one of a pair in a `choice`:
val ignoreInChoicePair = Vector("sic")


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

@main
def edtokens(f: String) = {
  val textLines = scala.io.Source.fromFile(f).getLines.toVector
  val xmlChunks = textLines.map {s => extractText(s)}.filterNot(_.isEmpty)
  val stringified = xmlChunks.map { s => stringifyXml(s) }
  // Should split on both white space and on punctuation.
  // But this is going to need to be updated with a reasonable
  // class for working with Greek strings.
  // Simple first pass:  split on white space:
  val tokenized = stringified.map(_.split("[ ]+")).filterNot(_.isEmpty).toVector
  for(psg <- tokenized) {
    println(psg.mkString(" / "))    
  }
}
