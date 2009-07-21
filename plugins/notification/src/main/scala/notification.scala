import com.rabbitmq.streams.plugins.notification.NotificationServer
import net.sf.json.JSONObject

/**
 * Very thin wrapper around server.
 */
class notification(config: JSONObject) extends NotificationServer(config)