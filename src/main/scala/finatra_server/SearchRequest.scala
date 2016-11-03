package finatra_server

case class SearchRequest(dataStructuresString: Option[String],
                         mbImplsString: Option[String],
                         adtMethods: List[String])
