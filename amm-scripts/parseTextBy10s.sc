/*
An ammonite script to process text in a 2-column tab-delimited file named as a command-line parameter.
The first column should give a URN for the passage.
The second column should be string of simple text content extracted from an HMT archival XML edition.

The script collects unique words from the second column's contents and submits them to the perseus morphological service.
It writes its output to files in 10-word chunks, and in case something goes wrong (e.g., network connection drops) allows you to resume processing from a specified "decade" (10-word chunk) by including an integer cound as a second parameter.  E.g., including a second parameter value of 2 will skip the first two tens of words, and resume with the the third decade (21st word and following).

Usage:

    amm parseTextBy10s.sc FILENAME <DECADECOUNT>


*/

import scala.xml._
import scala.io.Source
import java.io._


/** Add leading 0s to string form of an
* Integer < 10,000 so that it can be sorted
* alphabetically.
*/
def pad(i: Int) = {
  if (i < 10) {
    "000"+ i
  } else if (i < 100) {
    "00" + i
  } else if ( i < 1000) {
    "0" + i
  } else {
    i.toString
  }
}



/** Main method: see comments at top of script.
*/
@main
def getMorpheusBy10s(fName: String, initial10 : Int = 0) = {
  val scholiaEx = scala.io.Source.fromFile(fName).getLines.toVector
  // sanity check:  only keep lines that have 2
  // tab-delimited co..umns.
  val filteredArray = scholiaEx.map(s => s.split("\t")).filter(_.size == 2)
  // Extact text content and filter out junk characters and empty entries
  val justSchol = filteredArray.map( a => a(1).replaceAll( "[\\{\\}\\\\>,\\[\\]\\.·⁑:\"·]+",""))
  val wordVect = justSchol.map(_.split(" ").filterNot(_.isEmpty))
  val allWords = wordVect.flatten
  val filteredWords = allWords.filterNot(_.matches("[A-Za-z0-9]+")).filterNot(_.contains("urn"))
  // Unique and sort:
  val uniqueWords = filteredWords.groupBy( w => w).map(_._1)
  val sortedWords = uniqueWords.toVector.sorted

  // integer number of decades (counts of ten) to process:
  val total10s = sortedWords.size / 10

  println("For total words " + sortedWords.size)
  // Cycle entries 10 at a time:
  for ( i <-  initial10 to total10s - 1) {
    println("Look at decade " + i)
    val tenWords = sortedWords.drop(i * 10).take(10)
    val fName = "decade" + pad(i) + ".tsv"

    val parsedResults = tenWords.map( w => {
      println ("\tTry word " + w)
      val analysis = parse(w)
      w + "\t" + analysis
    })

    val textString = parsedResults.mkString("\n") + "\n"
    //println("Analyses: " +  textString)
    val pw = new PrintWriter(new File(fName))
    pw.write(textString)
    pw.close
  }


  // Get any entries remaining after last complete 10:
  val lastWords = sortedWords.drop(total10s * 10)
  println("Remaining words: " + lastWords.size)
  val parsedResults = lastWords.map( w => {
    println ("\tTry word " + w)
    val analysis = parse(w)
    w + "\t" + analysis
  })
  val textString = parsedResults.mkString("\n") + "\n"
  //println("Analyses: " +  textString)

  val pw = new PrintWriter(new File("lastWords.tsv"))
  pw.write(textString)
  pw.close
}

/** Type-safe function submitting a URL request.
* Returns String content of requested URL or an
* error message.  String content is formatted as a single
* line of text.
*/
def  getMorphReply(request: String) : String = {
  var reply : String = ""
  try {
    reply = scala.io.Source.fromURL(request).mkString.replaceAll("\n"," ")
  } catch {
    case _ => reply = "Error from parsing service."
  }
  reply
}


/** Gets a reply from the perseus morphology service
* for a given word.
*/
def parse (s: String): String = {
  val baseUrl = "https://services.perseids.org/bsp/morphologyservice/analysis/word?lang=grc&engine=morpheusgrc&word="
  val request = baseUrl + s
  getMorphReply(request)
}
