package org.homermultitext

/** Library for analyzing contents of HMT project archive.
*/
package object analytics {
  import io.github.cite_architecture.cite._

  trait HmtToken {
    val urn: CtsUrn
    val deformance: String
    val analysis: CiteUrn
  }

  trait HmtTokenizer {
    def tokenize(txt: TupleText)
  }
  type HmtTokenization = Vector[HmtToken]
  type TupleText = (String, String)
}
