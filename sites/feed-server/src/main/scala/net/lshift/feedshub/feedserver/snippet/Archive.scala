package net.lshift.feedshub.feedserver.snippet

import net.lshift.feedshub.feedserver.model.{LocalServer}
import scala.xml.NodeSeq

class Archive {
  def list : NodeSeq = {
      <ul>
      {LocalServer.archives map(
          archive =>
          <li>
              {archive.title}
              <a href={archive.name+".rss"}>RSS</a>
              <a href={archive.name+".atom"}>Atom</a>
          </li>
        )}
      </ul>
  }
}

