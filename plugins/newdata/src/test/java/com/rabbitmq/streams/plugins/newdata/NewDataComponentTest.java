package com.rabbitmq.streams.plugins.newdata;

import com.rabbitmq.streams.harness.*;
import net.sf.json.JSONObject;
import org.junit.After;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Mockito.*;

import java.io.IOException;

public class NewDataComponentTest {
  private NewDataComponent component;
  private MessageChannel messageChannel;
  private InputMessage message;
  private DatabaseResource database;
  private Logger logger;
  private static final String MESSAGE_BODY = "1234";

  @Before
  public void setup() {
    component = new NewDataComponent();
    messageChannel = mock(MessageChannel.class);
    component.setMessageChannel(messageChannel);

    database = mock(DatabaseResource.class);
    component.setDatabase(database);

    message = mock(InputMessage.class);
    logger = mock(Logger.class);
    component.setLog(logger);
  }

  @After
  public void teardown() throws MessagingException {

  }

  @Test
  public void shouldConfigure() throws PluginBuildException, PluginException {
    component.configure(null);
  }

  @Test
  public void shouldPassFirstMessageAndIgnoreSecond() throws PluginBuildException, PluginException, IOException {
    component.configure(null);

    when(message.body()).thenReturn(MESSAGE_BODY.getBytes());
    String key = new String(component.digest(MESSAGE_BODY.getBytes()));

    when(database.getDocument(key)).thenReturn(null).thenReturn(new JSONObject());

    component.input.handleMessage(message);
    component.input.handleMessage(message);
    
    verify(messageChannel, times(1)).publish("output", message);
  }

  @Test
  public void shouldThrowExceptionIfDatabaseUnavailable() throws PluginBuildException, IOException, MessagingException {

    component.configure(null);

    //noinspection ThrowableInstanceNeverThrown
    when(database.getDocument(anyString())).thenThrow(new IOException("BANG!"));
    try {
      component.input.handleMessage(message);
      fail("A plugin exception should have been thrown");
    }
    catch (PluginException ignore) {
    }

    verify(messageChannel, never()).publish(anyString(), any(Message.class));
  }

}
