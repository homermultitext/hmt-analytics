package org.homermultitext.analytics

import io.github.cite_architecture.cite._

import org.specs2.mutable.Specification


class TestHmtToken extends Specification {


  "Specification for HMT token object" >> {
    "which should have a CTS URN" >> {
      val urn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.msA:1.1@μῆνιν")
      val deformedText = "μῆνιν"
      val analysis = CiteUrn("urn:cite:hmt:lexicalitem.X")

      val token = HmtToken(urn,deformedText,analysis)
      token.urn.toString() == "urn:cts:greekLit:tlg0012.tlg001.msA:1.1@μῆνιν"

    }
    "which should have an analysis URN" >> {
      val urn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.msA:1.1@μῆνιν")
      val deformedText = "μῆνιν"
      val analysisUrn = CiteUrn("urn:cite:hmt:lexicalitem.X")

      val token = HmtToken(urn,deformedText,analysisUrn)
      token.analysis == CiteUrn("urn:cite:hmt:lexicalitem.X")

    }
    "which should have a text deformation" >> {
      val urn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.msA:1.1@μῆνιν")
      val deformedText = "μῆνιν"
      val analysis = CiteUrn("urn:cite:hmt:lexicalitem.X")

      val token = HmtToken(urn,deformedText,analysis)
      token.deformance.size ==  5

    }
  }
}
