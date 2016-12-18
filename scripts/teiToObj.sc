// read hmt tei and instantiate as object.
// Simple script to play with while planning
// how to develop a serious, tested model.
//
// Use a single global buffer for tokens, to
// avoid memory issues.  Ugly.
//
import scala.xml._
import scala.collection.mutable.ArrayBuffer

sealed trait LexicalCategory {def name : String}
case object LexicalToken extends LexicalCategory {val name = "lexical token"}
case object NumericToken extends LexicalCategory {val name = "numeric token"}
case object Punctuation extends LexicalCategory {val name = "punctuation"}
case object LiteralToken extends LexicalCategory {val name = "string literal"}



sealed trait EditorialStatus {def name : String}
case object Clear extends EditorialStatus {val name = "clear"}
case object Unclear extends EditorialStatus {val name = "unclear"}
case object Restored extends EditorialStatus {val name = "restored"}


case class EditedString (
  val str: String,
  val status: EditorialStatus
)
object EditedString {
  def typedText(es: EditedString) = es.str + " (" + es.status + ")"
}


case class HmtToken (
  var urn: String,
  var lang : String = "grc",
  var txtV: Vector[EditedString],
  var lexicalCategory: LexicalCategory,
  var editorialStatus: EditorialStatus = Clear
)


var tokenBuffer = scala.collection.mutable.ArrayBuffer.empty[HmtToken]
var wrappedWordBuffer = scala.collection.mutable.ArrayBuffer.empty[EditedString]




//val ignoreList = Vector("note")
//val wrapperList = Vector("choice", "cit")
val punctuation = Vector(",",".",";","⁑")


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

def getAlternate (hmtToken: HmtToken, n: xml.Node) = {
  println("Need to extract alternate")
}

// collect a mutable array (ArrayBuffer)
// of EditedString objects
def collectWrappedWordStrings(editorialStatus: EditorialStatus, n: xml.Node): Unit = {
  println("Need to collect wrapped node !" )

  n match {
    case t: xml.Text => {
      wrappedWordBuffer += EditedString(t.text, editorialStatus)
    }
    case e: xml.Elem => {
      println("Collect from node node  " + n.label)
      e.label match {
        case "unclear" => {
          for (ch <- e.child) {
            collectWrappedWordStrings(Unclear,ch)
          }
        }
        case _ => {
          for (ch <- e.child) {
            collectWrappedWordStrings(editorialStatus,ch)
          }
        }
      }
    }
  }
}

def collectTokens (hmtToken: HmtToken,  n: xml.Node ): Unit = {
  // xml nodes are either Text or Element:
  n match {
    case t: xml.Text => {
      // the awesomeness of regex:
      // split on set of characters without
      // losing them:
      val depunctuate =   t.text.split("((?<=[,;⁑\\.])|(?=[,;⁑\\.]))")

      val tokenList = depunctuate.flatMap(_.split("[ ]+").filterNot(_.isEmpty))
      for (tk <- tokenList) {
        val edStr = EditedString(tk, Clear)
        var currToken = hmtToken.copy(txtV = Vector(edStr))
        if (punctuation.contains(tk)) {
          currToken.lexicalCategory = Punctuation
        }
        tokenBuffer += currToken
      }
    }

    case e: xml.Elem => {
      println("Analyze node  " + n.label)
      e.label match {
        case "note"=> {/*ignore*/}

        case "w" => {
          println("Node is a w" )
          wrappedWordBuffer.clear
          collectWrappedWordStrings(Clear,e)
          var currToken = hmtToken.copy(txtV = wrappedWordBuffer.toVector)
          tokenBuffer += currToken
        }

        case "choice" => {
          val alt = getAlternate(hmtToken,e)
        }

        case _ =>  {
          for (ch <- e.child) {
            collectTokens(hmtToken, ch)
          }
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
    txtV = Vector.empty
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
      println(tk.urn + " " + tk.lexicalCategory + " "+ tk.txtV.map(EditedString.typedText(_)).mkString(" + "))
    }
  }
}
