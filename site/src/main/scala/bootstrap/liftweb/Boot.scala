package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

import net.liftweb.widgets.tablesorter.TableSorter

import net.lshift.feedshub.management.controller._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("net.lshift.feedshub.management")
    TableSorter.init

    // Build SiteMap
    val entries = 
                  Menu(Loc("Home", List("index"), "Home")) ::
                  Menu(Loc("Feeds", List("feeds"), "Feeds")) :: Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
    Feeds ! Init("http://localhost:5984")
  }
}
