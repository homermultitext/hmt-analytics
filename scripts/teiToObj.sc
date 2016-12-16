// read hmt tei and instantiate as object.
//
// Use a single global
// buffer for tokens?
//
import scala.xml._
import scala.collection.mutable.ArrayBuffer

sealed trait LexicalCategory {def name : String}
case object LexicalToken extends LexicalCategory {val name = "lexical token"}
case object NumericToken extends LexicalCategory {val name = "numeric token"}
case object Punctuation extends LexicalCategory {val name = "punctuation"}
case object LiteralToken extends LexicalCategory {val name = "string literal"}



case class HmtToken (
  var urn: String,
  var lang : String = "grc",
  var txt: String,
  var lexicalCategory: LexicalCategory)


var tokenBuffer = scala.collection.mutable.ArrayBuffer.empty[HmtToken]



val ignoreList = Vector("note")
val wrapperList = Vector("w", "choice", "cit")

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

def collectTokens (hmtToken: HmtToken,
  n: xml.Node ): Unit = {


  n match {
    case t: xml.Text => {

      val depunctuate =   t.text.split("((?<=[,;⁑\\.])|(?=[,;⁑\\.]))")



      val tokenList = depunctuate.flatMap(_.split("[ ]+").filterNot(_.isEmpty))


      for (tk <- tokenList) {
        var currToken = hmtToken.copy(txt = tk)
        tokenBuffer += currToken
        //println("Add token " + currToken.txt)
        //println(" -> buffer " + tokenBuffer)
      }
      //currToken.txt += t
      //println("Update token: " + currToken.txt)
    }
    case e: xml.Elem => {
      println("Analyze node  " + n.label)
      if (ignoreList.contains(e.label)) {

      } else if (wrapperList.contains(e.label)) {
        println("Wrapper element: " + e.label)

      } else {
        for (ch <- e.child) {
          collectTokens(hmtToken, ch)
        }
      }
    }
  }
}

def tokenizePair(urnStr: String, xmlStr: String ) = {
  val root  = XML.loadString(xmlStr)
  val currToken = HmtToken(
    urn = urnStr,
    lexicalCategory = LexicalToken,
    txt = ""
  )
  // zero out the global buffer,
  // then collect:
  tokenBuffer.clear
  collectTokens(currToken, root)
  var count = 1
  for (tk <- tokenBuffer) {
    tk.urn = tk.urn + "." + count
    count = count + 1
  }
  tokenBuffer.toVector
}



@main
def edtokens(f: String) = {
  val textLines = scala.io.Source.fromFile(f).getLines.toVector.filterNot(_.isEmpty)
  val extracts = textLines.map( s => (extractUrn(s), extractText(s) ))
  val pairs = extracts.filterNot(_._2.isEmpty)

  val urTokens = pairs.map {
    case (urn, xmlStr) => tokenizePair(urn,xmlStr)
  }
  for (psg <- urTokens) {
    for (tk <- psg) {
    println(tk.urn + " " + tk.txt)
  }
  }
/*
  for (pr <- pairs) {
    tokenizePair(pr._1, pr._2)
  }

  println("Total tokens: " + tokenBuffer.size)
  for (tk <- tokenBuffer) {
    println(tk.urn + " == " + tk.txt)
  }
*/

}
