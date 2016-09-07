#!/usr/bin/env amm

// Indexes n attribute value on a given element

import scala.io.Source
import scala.xml.XML

@main
def indexNAttr(elemName: String, fName: String) {
  val lns = Source.fromFile(fName).getLines.toList
  for (l <- lns) {
    val arr = l.split("\t")
    val n = scala.xml.XML.loadString(arr(1))
    val elems = n \\ elemName
    for (e <- elems) {
      println(arr(0) + "\t" + (e \ "@n"))
    }
  }

}
