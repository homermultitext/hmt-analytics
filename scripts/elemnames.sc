#!/usr/bin/env amm
// read a file with one unit of WF xml per line.
// find set of unique element names.
// Not very robust.  Fails on bad input such
// as empty lines.
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.xml.XML

// List of unique element names
var elemNames = new ListBuffer[String]()

// simple recursive method to find names
// of all elements
def walkTree(n: xml.Node): Unit = {
    n match {
      case t: xml.Text => ""
      case e: xml.Elem => {
        if (elemNames contains e.label) {
        } else {
          elemNames += e.label //+ " => " + e.namespace
        }
        for (ch <- e.child) {
          walkTree(ch)
        }
      }
    }
  }

@main
def browse(fName: String) {
  var count = 0
  for (line <- Source.fromFile(fName).getLines) {
    count = count + 1

    val n = scala.xml.XML.loadString(line)
    walkTree(n)
  }
  for (e <- elemNames) { println(e)}
}
