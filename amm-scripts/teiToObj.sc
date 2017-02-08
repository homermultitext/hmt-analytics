// read hmt tei and instantiate as object.
// Simple script to play with while planning
// how to develop a serious, tested model.
//
// Use a single global buffer for tokens,
// and a single global buffer for wrapped word
// tokens, in order to avoid memory issues
// when recursively passing big buffers
// down a deep xml stack. Ugly.
//
import scala.xml._
import scala.collection.mutable.ArrayBuffer


// This should be externally defined.
val punctuation = Vector(",",".",";","⁑")

sealed trait LexicalCategory {def name : String}
case object LexicalToken extends LexicalCategory {val name = "lexical token"}
case object NumericToken extends LexicalCategory {val name = "numeric token"}
case object Punctuation extends LexicalCategory {val name = "punctuation"}
case object LiteralToken extends LexicalCategory {val name = "string literal"}



sealed trait AlternateCategory {def name : String}
case object Restoration extends AlternateCategory {val name = "editorial restoration or completion"}
case object Multiform extends AlternateCategory {val name = "scribally recorded multiform"}
case object Correction extends AlternateCategory {val name = "scribal correction"}
case object Original extends AlternateCategory {val name = "no alternate reading"}

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

case class AlternateReading (
  var alternateCategory: AlternateCategory,
  var txtV: Vector[EditedString]
)
object AlternateReading {
  def alternative (alt: AlternateReading) = {
    alt.alternateCategory match {
      case Original => "(no alternate)"
      case _  => alt.txtV.map(es => EditedString.typedText(es)).mkString(" + ")
    }
  }
}

val defaultAlternate = AlternateReading(Original, Vector.empty[EditedString])



sealed trait DiscourseCategory {def name : String}
case object DirectVoice extends DiscourseCategory {val name = "voice of text"}
case object QuotedLanguage extends DiscourseCategory {val name = "quoted language"}
case object QuotedLiteral extends DiscourseCategory {val name = "quoted literal string"}
case object CitedText extends DiscourseCategory {val name = "cited passage of text"}


case class HmtToken (  var urn: String,
  var lang : String = "grc",
  var txtV: Vector[EditedString],
  var lexicalCategory: LexicalCategory,

  var lexicalDisambiguation: String = "Automated disambiguation",
  var alternateReading: AlternateReading = defaultAlternate,
  var discourse: DiscourseCategory = DirectVoice
)



var tokenBuffer = scala.collection.mutable.ArrayBuffer.empty[HmtToken]
var wrappedWordBuffer = scala.collection.mutable.ArrayBuffer.empty[EditedString]





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

// n must be a TEI choice element
// possibilities;
// abbr/expan
// sic/corr
// orig/reg
def getAlternate (hmtToken: HmtToken, n: xml.Node) = {
  println("=>Need to extract alternate")
  val cNames = n.child.map(_.label).distinct.filterNot(_ == "#PCDATA")
  println("CHILDREN " + cNames)
  /*
  for (c <- n.child) {
    c.label match {
      case "expan" => {
        println("WE'RE ExPANDING")
        val expansion = EditedString(c.text.replaceAll(" ",""), Restored)
        val alt = AlternateReading(Restoration, Vector(expansion))
        println("\tAlt is " + alt)
        var currToken = hmtToken.copy(alternateReading = alt )
        println("Token is " + currToken)
        tokenBuffer += currToken
      }
      case _ => println("=>NOT YET IMPLENTED: " + c.label)
    }
  }*/
}

// collect a mutable array (ArrayBuffer)
// of EditedString objects
def collectWrappedWordStrings(editorialStatus: EditorialStatus, n: xml.Node): Unit = {
  n match {
    case t: xml.Text => {
      wrappedWordBuffer += EditedString(t.text.replaceAll(" ", ""), editorialStatus)
    }

    case e: xml.Elem => {
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
      e.label match {
        case "note"=> {/*ignore*/}
        case "ref"=> {/*ignore*/}

        case "w" => {
          wrappedWordBuffer.clear
          collectWrappedWordStrings(Clear,e)
          var currToken = hmtToken.copy(txtV = wrappedWordBuffer.toVector)
          tokenBuffer += currToken
        }

        case "choice" => {
          val alt = getAlternate(hmtToken,e)

        }

        case "cit" => {
          println("WRAPPER: cit")
        }

        case l: String =>  {
          for (ch <- e.child) {
            ch.label match {
              case "q" => {
                val quotedToken = hmtToken.copy(discourse = QuotedLanguage)
                for (quoted <- ch.child) {
                  collectTokens(quotedToken, quoted)
                }
              }

              case "persName" => {
                val nList = ch \ "@n"
                val pnToken = hmtToken.copy(lexicalDisambiguation = nList(0).text)
                for (pnVal <- ch.child) {
                  collectTokens(pnToken, pnVal)
                }
              }

              case _ => {
                if ((l == "p") || (l == "div")) {} else {
                  println("Getting child of " + l )
                }
                collectTokens(hmtToken, ch)
              }
            }

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
      println(tk.urn + " " + tk.lexicalCategory + " " + tk.lang + " " + tk.discourse + " " + tk.lexicalDisambiguation + " " + AlternateReading.alternative(tk.alternateReading) + " " + tk.txtV.map(EditedString.typedText(_)).mkString(" + "))
    }
  }
}
