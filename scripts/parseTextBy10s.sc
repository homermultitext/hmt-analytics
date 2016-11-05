import scala.xml._
import scala.io.Source
import java.io._

@main
def getMorpheusBy10s(fName: String, initial10 : Int) = {

  // Read in a 2-column tab-delmited file. 2nd column should be
  // a string of text content extracted from archival XML edition.
  val scholiaEx = scala.io.Source.fromFile(fName).getLines.toVector
  val filteredArray = scholiaEx.map(s => s.split("\t")).filter(_.size == 2)
  // Extact text content and filter out junk characters and empty entries
  val justSchol = filteredArray.map( a => a(1).replaceAll( "[\\{\\}\\\\>,\\[\\]\\.·⁑:\"·]+",""))
  val wordVect = justSchol.map(_.split(" ").filterNot(_.isEmpty))
  val allWords = wordVect.flatten
  val filteredWords = allWords.filterNot(_.matches("[A-Za-z0-9]+")).filterNot(_.contains("urn"))
  // Unique and sort:
  val uniqueWords = filteredWords.groupBy( w => w).map(_._1)
  val sortedWords = uniqueWords.toVector.sorted



  // integer number of tens to process:
  val total10s = sortedWords.size / 10



  println("For total words " + sortedWords.size)
  // Cycle entries 10 at a time:
  for ( i <-  initial10 to total10s - 1) {

    println("Look at decade " + i)
    val tenWords = sortedWords.drop(i * 10).take(10)
    val fName = "decade" + i + ".tsv"

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


  // Get any remaining entries at last full 10:
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

def  getMorphReply(request: String) : String = {
  var reply : String = ""
  try {
    reply = scala.io.Source.fromURL(request).mkString.replaceAll("\n"," ")
  } catch {
    case _ => reply = "Error from parsing service."
  }
  reply
}


def parse (s: String): String = {
  val baseUrl = "https://services.perseids.org/bsp/morphologyservice/analysis/word?lang=grc&engine=morpheusgrc&word="
  val request = baseUrl + s
  getMorphReply(request)
}
