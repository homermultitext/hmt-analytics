#!/usr/bin/env amm

import scala.xml.XML

// simple recursive method to print
// all text of an xml node.
def walkTree(n: xml.Node): Unit = {
    n match {
      case t: xml.Text => println(t)
      case e: xml.Elem => {
        for (ch <- e.child) {
          walkTree(ch)
        }
      }
    }
  }

def getNextUrn(n: xml.Elem) = {
  val urn = n \\ "next" \ "urn"
  urn.text
}

val baseUrl = "http://www.homermultitext.org/hmt-digital/texts?request=GetPassagePlus&context=10&urn="

def quoteUrn(urn: String) {
  val url = baseUrl + urn
  println("Requesting " + url + "...\n")
  val ctsReply = XML.load(url)
  val psg = ctsReply \\ "passage"
  val nxt = getNextUrn(ctsReply)

  for (n <- psg) { walkTree(n) }
  println ("\nNext: " + nxt)
  val goAhead = readLine("More?  ")
  goAhead.toLowerCase.charAt(0) match {
    case 'y' => quoteUrn(nxt)
    case _ => println("Done.")
  }
}

@main
def browse(urn: String) {
  quoteUrn(urn)
}
