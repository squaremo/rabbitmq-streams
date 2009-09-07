package bootstrap.liftweb

import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.widgets.tablesorter.TableSorter

class Boot {
  def boot {
    LiftRules.addToPackages("com.rabbitmq.streams.management")
    TableSorter.init

    // Build SiteMap
    val makeLiftAwareOfNonMenuedPagesWhenUsingSiteMap = Menu(Loc("archive", List("archive") -> true, "archive")) :: Nil
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
