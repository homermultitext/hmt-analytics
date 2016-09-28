package org.homermultitext


package analytics {
  import io.github.cite_architecture.cite._

  object HmtEditorialTokenizer extends HmtTokenizer  {
      def tokenize(textVector: TupleText) = {
        val pairs = textVector.map { case(k,v) => (CtsUrn(k), v)}

        for (pr <- pairs) {

          println(pr._1.passageNodeSubrefText + " some type.")

        }
        def u = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.msA:1.1@μῆνιν")
        def deformed = "μῆνιν"
        def analysis =  CiteUrn("urn:cite:hmt:lextoken.N")
        def v = Vector ( HmtToken(u, deformed,analysis) )
        
        v
      }
  }
}
