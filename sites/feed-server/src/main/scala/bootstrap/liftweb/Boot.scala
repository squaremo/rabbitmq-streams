package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

import com.rabbitmq.streams.feedserver.view.OutputFeed
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("com.rabbitmq.streams.feedserver")

    // Build SiteMap
    val entries = Menu(Loc("home", List("index"), "Home")) :: Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
    LiftRules.dispatch.append {
        case Req(name :: Nil, "rss", GetRequest) =>
            () => OutputFeed.rss(name)
        case Req(name :: Nil, "atom", GetRequest) =>
            () => OutputFeed.atom(name)
    }
  }
}
