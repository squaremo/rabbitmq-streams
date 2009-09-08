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
    val hidden = Menu(Loc("HIDDEN", List("HIDDEN"), "HIDDEN", Hidden), makeLiftAwareOfNonMenuedPagesWhenUsingSiteMap: _*)

    val entries =
    hidden ::
    Menu(Loc("home", List("index"), "Home")) ::
    Menu(Loc("feeds", List("feeds"), "Feeds")) ::
    Menu(Loc("terminals", List("terminals"), "Sources and Destinations")) ::
    Menu(Loc("archives", List("archives"), "Archives")) :: Nil

    LiftRules.setSiteMap(SiteMap(entries: _*))
  }
}

object Boot {
  private var couch = new Object with CouchConfig {}
  def couchConfig:CouchConfig = couch
  def couchConfig(config: CouchConfig) {
    couch = config
  }

  private var rabbit = new Object with RabbitConfig
  def rabbitConfig:RabbitConfig = rabbit
  def rabbitConfig(config: RabbitConfig) {
    rabbit = config
  }
}

trait CouchConfig {
  def server = LiftRules.context.getInitParameter("couch_server")
  def port = LiftRules.context.getInitParameter("couch_port").toInt
  def username = LiftRules.context.getInitParameter("couch_user")
  def password = LiftRules.context.getInitParameter("couch_password")
  def url = "http://" + server + ":" + port
  def database = LiftRules.context.getInitParameter("couch_database")
}

trait RabbitConfig {
  def server = LiftRules.context.getInitParameter("rabbit_server")
  def port = LiftRules.context.getInitParameter("rabbit_port").toInt
  def username = LiftRules.context.getInitParameter("rabbit_user")
  def password = LiftRules.context.getInitParameter("rabbit_password")
}
