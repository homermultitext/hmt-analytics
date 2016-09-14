#!/usr/bin/env amm

// Ammonite script to tokenize a 2-column file.
// Tokenizes column 2.

import scala.io.Source
import scala.xml.XML


def punctSplit(s: String) {
  val spaced = s.replaceAll("([.;,])", "")
  val crunch = s.split("[ \t]+").mkString
  //println("\t\t(" + crunch + "))")
  crunch
}




// simple recursive method to collect
// all text of an xml node.
def normalizeText(n: xml.Node, inWord : Boolean, cumulative: String): String = {
  var res = cumulative
  //println("STARTING VALUE: " + res + ", in word = " + inWord)
  n match {
    case t: xml.Text => if (inWord) {

      res += t.toString().replaceAll(" ", "")
      //println("INWORD: add '" +  t.toString().replaceAll(" ", "") + "' to get " + res)
    } else {
      res += " " + t.toString()
    }

    case e: xml.Elem => {
      //println("ELEM " + e.label)
      e.label match {
        case "ref" => res = cumulative
        case "note" => res = cumulative
        case "w" => for (ch <- e.child) {
          res += normalizeText(ch, true, cumulative )
        }
        case _ => for (ch <- e.child) {
          res += normalizeText(ch, inWord, cumulative)
        }
      }
      //println("AFTER "  + e.label + ", value = " + res)
    }
  }

  res
}




@main
def normalizedTokens(fName: String) {
  val lns = Source.fromFile(fName).getLines.toList
  for (l <- lns) {
    val arr = l.split("\t")
    val n = scala.xml.XML.loadString(arr(1))
    val tokens = (arr(0), normalizeText(n,false,""))
    println (tokens)
  }

}
