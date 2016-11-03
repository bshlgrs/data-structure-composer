package finatra_server

import com.twitter.finagle.http.{Response, Request}
import com.twitter.finatra.http.HttpServer
import com.twitter.finatra.http.filters.CommonFilters
import com.twitter.finatra.http.routing.HttpRouter


object SearchServerMain extends SearchServer

class SearchServer extends HttpServer {

  override def configureHttp(router: HttpRouter) {
    router
      .filter[CommonFilters]
      .add[SearchController]
  }
}
