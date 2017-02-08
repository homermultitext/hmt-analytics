#!/usr/bin/env amm

import scala.io.Source

// Two parameters required:
// 1. name of file with list of URN + tokens
// 2. name of file with list of token + lemmatization

@main
def lemmatize(scholionTokenPair: String, scholiaLemmatization: String) = {
  // eg., "scholia-tokens-nfc.tsv"
  val tokenPairs =  Source.fromFile(scholionTokenPair).getLines.toVector.map(_.split("\t"))


  // e.g., "scholia-lemmatized-nonulls-nfc.txt"
  val tokenLexEntArray = Source.fromFile(scholiaLemmatization).getLines.toVector.map(_.split("\t"))

  for (t <- tokenPairs) {
    val token =  t(1)
    val scholion = t(0)
    val lexEnt = tokenLexEntArray.find(_(0) == token)
    print (scholion + "\t" + token)
    lexEnt match {
      case None =>  println ("\tnone")
      case Some(l) => println("\t" + l.toVector(1))
    }



    //println (scholion + ": " + token + " -> " + lexEnt)
  }
}
