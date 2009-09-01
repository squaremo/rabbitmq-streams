import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.NotificationType;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.PluginException;
import com.rabbitmq.streams.harness.Server;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.util.*;

public class email_sender extends Server {

  private Map<String, List<EmailDestination>> destinationsMap = new HashMap<String, List<EmailDestination>>();
  String host, username, password, transportProtocol;
  boolean useTls = false;

  public final Server.ServerInputReader input = new Server.ServerInputReader() {

    public void handleBodyForTerminal(byte[] body, String key, InputMessage ack) throws PluginException {
      log.info("Handling message");

      List<EmailDestination> dests = destinationsMap.get(key);
      if (null != dests) {
        try {
          for (EmailDestination dest : dests) {
            dest.send(body);
          }
        }
        catch (MessagingException e) {
          notifier.notify(NotificationType.Unavailable, "Could not deliver email " + e.getMessage());
          throw new PluginException(e);
        }

      }
      ack.ack();
    }
  };

  protected void terminalStatusChange(String terminalId,
    List<JSONObject> terminalConfigs, boolean active) {

    if (active) {
      List<EmailDestination> dests = destinationsMap.get(terminalId);
      if (null == dests || 0 == dests.size()) {
        try {
          dests = new ArrayList<EmailDestination>(terminalConfigs.size());
          for (JSONObject termConfigObject : terminalConfigs) {
            JSONObject destConfig = termConfigObject.getJSONObject("destination");
            EmailDestination dest = new EmailDestination(destConfig);
            dests.add(dest);
          }
          destinationsMap.put(terminalId, dests);
        }
        catch (AddressException e) {
          email_sender.this.log.error(e);
        }
      }
    }
    else {
      destinationsMap.remove(terminalId);
    }
  }

  @Override
  public void configure(JSONObject config) throws PluginBuildException {
    super.configure(config);
    registerInput(input);

    host = config.getString("host");
    username = config.getString("username");
    password = config.getString("password");
    transportProtocol = config.getString("transportProtocol");
    useTls = config.optBoolean("TLS", false);

    log.info(String.format("Setup config vars, host: %s, username: %s, password: %s, protocol: %s, TLS: %s", host, username, password, transportProtocol, useTls));
  }

  private class EmailDestination {
    private InternetAddress[] addresses;
    private String subject;

    public EmailDestination(JSONObject destConfig) throws AddressException {
      log.info("Creating new email destination");
      JSONArray to = destConfig.getJSONArray("to");
      addresses = new InternetAddress[to.size()];

      for (int i = 0; i < to.size(); i++) {
        addresses[i] = new InternetAddress(to.getString(i));
      }

      subject = destConfig.optString("subject", "");
    }

    public void send(byte[] msg) throws MessagingException {
      sendMail(host, username, password, subject, new String(msg), addresses);
    }

    private void sendMail(String host, String username, String password, String subject, String body, InternetAddress[] to) throws MessagingException {
      log.info("Sending mail");

      Properties props = new Properties();
      if (useTls) {
        props.put("mail.smtp.starttls.enable", "true");
      }
      props.setProperty("mail.transport.protocol", transportProtocol);
      props.setProperty("mail.host", host);
      props.setProperty("mail.user", username);
      props.setProperty("mail.password", password);

      Session mailSession = Session.getDefaultInstance(props, null);
      Transport transport = mailSession.getTransport();

      MimeMessage message = new MimeMessage(mailSession);
      message.setSubject(subject);
      message.setContent(body, "text/plain");
      message.addRecipients(Message.RecipientType.TO, to);

      transport.connect();
      transport.sendMessage(message, message.getRecipients(Message.RecipientType.TO));
      transport.close();
    }
  }
}
