import com.rabbitmq.streams.harness.InputMessage;
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
          dests = new ArrayList<EmailDestination>(terminalConfigs
            .size());
          for (JSONObject termConfigObject : terminalConfigs) {
            JSONObject destConfig = termConfigObject
              .getJSONObject("destination");
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

    JSONObject c = config.getJSONObject("configuration");
    host = c.getString("host");
    username = c.getString("username");
    password = c.getString("password");
    transportProtocol = c.getString("transportProtocol");

    log.info(String.format("Setup config vars, host: %s, username: %s, password: %s, protocol: %s", host, username, password, transportProtocol));
  }

  private class EmailDestination {
    private Collection<InternetAddress> to;

    public EmailDestination(JSONObject destConfig) throws AddressException {
      JSONArray jsonTo = destConfig.getJSONArray("to");
      to = new ArrayList<InternetAddress>();

      log.info("Creating new email destination");

      for (int i = 0; i < jsonTo.size(); i++) {
        to.add(new InternetAddress(jsonTo.getString(i)));
      }
    }

    public void send(byte[] msg) throws MessagingException {
      //todo: set the subject from headers
      sendMail(host, username, password, "The subject, to be extracted from headers", new String(msg), to);
    }

    private void sendMail(String host, String username, String password,
      String subject, String body, Collection<InternetAddress> to)
      throws MessagingException {
      log.info("Sending mail");

      Properties props = new Properties();
      props.setProperty("mail.transport.protocol", transportProtocol);
      props.setProperty("mail.host", host);
      props.setProperty("mail.user", username);
      props.setProperty("mail.password", password);

      Session mailSession = Session.getDefaultInstance(props, null);
      Transport transport = mailSession.getTransport();

      MimeMessage message = new MimeMessage(mailSession);
      message.setSubject(subject);
      message.setContent(body, "text/plain"); //todo: set content mime type from config/headers?


      //todo: config recipients from message headers?
      for (InternetAddress t : to) {
        message.addRecipient(Message.RecipientType.TO, t);
      }

      //todo: Add cc and bcc?

      transport.connect();
      transport.sendMessage(message, message
        .getRecipients(Message.RecipientType.TO));
      transport.close();
    }
  }
}
