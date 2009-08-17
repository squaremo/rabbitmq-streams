package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("com.rabbitmq.streams.management")
    TableSorter.init

    // Build SiteMap
    val makeLiftAwareOfNonMenuedPagesWhenUsingSiteMap =
      Menu(Loc("archive", List("archive") -> true, "archive")) :: Nil
    val hidden = Menu(Loc("HIDDEN", List("HIDDEN"), "HIDDEN", Hidden), makeLiftAwareOfNonMenuedPagesWhenUsingSiteMap:_*)

    val entries =
      hidden ::
      Menu(Loc("home", List("index"), "Home")) ::
      Menu(Loc("feeds", List("feeds"), "Feeds")) ::
      Menu(Loc("terminals", List("terminals"), "Sources and Destinations")) ::
      Menu(Loc("archives", List("archives"), "Archives")) :: Nil


    LiftRules.setSiteMap(SiteMap(entries: _*))
  }
}
