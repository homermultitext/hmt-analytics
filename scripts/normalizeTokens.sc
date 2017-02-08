#!/usr/bin/env amm

// Ammonite script to tokenize a 2-column file.
// Tokenizes column 2.

import scala.io.Source
import scala.xml.XML
import scala.collection.mutable.ArrayBuffer


import ammonite._, Resolvers._

//Add Houston to default set of resolvers:
val beta = Resolver.Http(
   "hu",
   "http://beta.hpcc.uh.edu/nexus/content/repositories/releases/",
   MavenPattern,
   true
 )
interp.resolvers() = interp.resolvers() :+ beta

// import Greek library for normalization
import $ivy.`io.github.neelsmith::greek:0.1.0`
import io.github.neelsmith.greek._

//import $ivy.`edu.unc.epidoc:transcoder:1.2-SNAPSHOT`
//import edu.unc.epidoc.transcoder._

def punctSplit(s: String) = {
  val spaced = s.replaceAll("([.;,])", "")
  val crunch = s.split("[ \t]+").mkString
  //println("\t\t(" + crunch + "))")
  crunch
}
/*        case "ref" => res = cumulative
        case "note" => res = cumulative*/


// simple recursive method to collect
// all text of an xml node.
def normalizeText(n: xml.Node, tokenType : String, cumulative: String): String = {
  val ignorable = Vector("abbr","del","note", "orig","ref","sic")


  var res = cumulative
  //println("STARTING VALUE: " + res + ", in word = " + inWord)
  n match {
    case t: xml.Text => if (tokenType == "w") {

      res += t.toString().replaceAll(" ", "")
      //println("INWORD: add '" +  t.toString().replaceAll(" ", "") + "' to get " + res)
    } else {
      res += " " + t.toString()
    }

    case e: xml.Elem => {
      //println("ELEM " + e.label)
      if (ignorable.contains(e.label)) {
        res = cumulative
      } else {
        e.label match {
          case "w" => for (ch <- e.child) {
            res += normalizeText(ch, "w", cumulative )
          }
          case _ => for (ch <- e.child) {
            res += normalizeText(ch, tokenType, cumulative)
          }
        }
      }
      //println("AFTER "  + e.label + ", value = " + res)
    }
  }

  //GreekStringN(res).unicodeView
  res
}




// Tokenize and normalize text in a 2-column file
@main
def normalizedTokens(fName: String) {
  val lns = Source.fromFile(fName).getLines.toList
  for (l <- lns) {
    val arr = l.split("\t")
    val n = scala.xml.XML.loadString(arr(1))
    val wds = normalizeText(n,"","").split("[⁑\\., ··:+a-zA-Z0-9;&]").filter(_.nonEmpty)
    for (w <- wds) {
      //val gw = GreekStringN(w)
    println ( arr(0) + "\t" + w)
    }

  }

}
