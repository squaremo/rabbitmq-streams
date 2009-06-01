package net.lshift.feedshub.feedserver.snippet

import net.lshift.feedshub.feedserver.model.{Server}
import scala.xml.NodeSeq

object LocalServer extends Server("http://localhost:5984", "feedshub_status")

class Archive {
  def list : NodeSeq = {
      <ul>
      {LocalServer.archives map(
          archive => <li>{archive.name}</li>
        )}
      </ul>
  }
}

