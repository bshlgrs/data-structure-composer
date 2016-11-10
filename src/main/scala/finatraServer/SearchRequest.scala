package finatraServer

case class SearchRequest(dataStructuresString: Option[String],
                         mbImplsString: Option[String],
                         adtMethods: List[String])
