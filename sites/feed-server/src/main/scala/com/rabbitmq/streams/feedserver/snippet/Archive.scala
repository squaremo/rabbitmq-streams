package com.rabbitmq.streams.feedserver.snippet

import com.rabbitmq.streams.feedserver.model.{LocalServer}
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

