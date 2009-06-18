/*
 * Subscriber.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.xmpppubsub.tools

import org.jivesoftware.smack.{XMPPConnection,ConnectionConfiguration,XMPPException}
import org.jivesoftware.smackx.pubsub._
import org.jivesoftware.smackx.pubsub.listener.ItemEventListener

import scala.collection.jcl.Conversions._

object Subscriber {

    def main(args : Array[String]) {
        val nodeId = if (args.length < 1) "/home/localhost/testuser/testnode" else args(0)
        val options = new ConnectionConfiguration("localhost", 5222)
        val connection = new XMPPConnection(options)
        connection.connect
        connection.login("testuser", "testuser")
        val pubsub = new PubSubManager(connection, "pubsub.localhost")
        val node : Node =
        (try {
                pubsub.getNode(nodeId)
            }
         catch {
                case e : XMPPException if (e.getXMPPError.getCode == 404) => {
                        val defopts = new ConfigureForm(FormType.submit)
                        pubsub.createNode(nodeId, defopts)
                        pubsub.getNode(nodeId)
                    }
                case e => throw e
            })
        // make sure we can subscribe
        val opts = new ConfigureForm(FormType.submit)
        opts.setAccessModel(AccessModel.open)
        node.sendConfigurationForm(opts)
        node.addItemEventListener(printer)
        val sub = node.subscribe("testuser")
        while (System.in.read != 0) {
        }
    }

    object printer extends ItemEventListener {
        def handlePublishedItems(items : ItemPublishEvent)
		{
			Seq(items.getItems:_*).foreach(item => println(item.toXML))
		}
    }

}
