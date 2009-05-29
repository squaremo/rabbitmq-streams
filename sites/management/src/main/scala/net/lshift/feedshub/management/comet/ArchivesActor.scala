package net.lshift.feedshub.management.comet


import liftweb.http._
import liftweb.util.Full
import liftweb.widgets.tablesorter.TableSorter

class ArchivesActor extends CometActor {

  var records: List[String] = Nil

  override def defaultPrefix = Full("a")

  override def render: RenderOut = {
    bind("list" ->(
      <table id="archive-list" class="tablesorter">
        <thead>
          <tr><th>Date</th><th>Archive</th></tr>
        </thead>
        <tbody>
          {records.map(r => <tr><td>Some date</td><td>{r}</td></tr>)}
        </tbody>
      </table>
      <script>{TableSorter.jsRender("feeds-list")}</script>))
  }
}