/*
 */

package com.rabbitmq.streams.harness;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.QueueingConsumer;
import java.io.IOException;
import net.sf.json.JSONObject;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import static org.mockito.Mockito.*;

/**
 *
 * @author mikeb@lshift.net
 */
public class MessageChannelTest {

  public MessageChannelTest() {
  }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

  @Before
  public void setUp() {
  }

  @After
  public void tearDown() {
  }

  /**
   * 
   */
  @Test
  public void testOverlappingConsume() throws Exception {
    Channel channel = mock(Channel.class);
    when(channel.isOpen()).thenReturn(true);

    final boolean[] inSyncMethod = {false};
    final String[] violation = {null};
    Answer checkNotInMethod = new Answer() {
      public Object answer(InvocationOnMock inv) {
        System.out.println("Entering " + inv.getMethod().getName());
        if (inSyncMethod[0]) violation[0] = "Violation in " + inv.getMethod().getName();
        inSyncMethod[0] = true;
        try {
          Thread.sleep(200);
        }
        catch (InterruptedException ex) {
          // pass
        }
        inSyncMethod[0] = false;
        System.out.println("Exiting " + inv.getMethod().getName());
        return null;
      }
    };
    // make all of these both delay and check that nothing's already using the channel
    doAnswer(checkNotInMethod).when(channel).basicAck(anyLong(), anyBoolean());
    doAnswer(checkNotInMethod).when(channel).txSelect();
    doAnswer(checkNotInMethod).when(channel).basicPublish(anyString(), anyString(), any(BasicProperties.class), eq("foo".getBytes()));

    // FIXME get from pluginbuilder
    AMQPMessageChannel ms = new AMQPMessageChannel(channel, new JSONObject(), mock(Logger.class), false);

    // In bug21481, when there were two input handlers, the second consume reported an IllegalStateException.
    // My hypothesis is that it was because there was a mesasge waiting on the first channel, and it was
    // publishing, acking or committing at the same time as the second consume.
    ms.declareQueue("input1", "input1");
    ms.declareQueue("input2", "input2");

    // Snaffle the queueingConsumer and send it a message when we get told about it
    final QueueingConsumer[] q = {null};
    doAnswer(new Answer() {
      public String answer(InvocationOnMock inv) throws IOException {
        q[0] = (QueueingConsumer) inv.getArguments()[1];
        byte[] body = "foo".getBytes();
        q[0].handleDelivery("tag", mock(Envelope.class), new BasicProperties(), body);
        return "tag";
      }
    }).when(channel).basicConsume(eq("input1"), any(QueueingConsumer.class));

    doAnswer(checkNotInMethod).when(channel).basicConsume(eq("input2"), any(QueueingConsumer.class));

    InputHandler h = mock(InputHandler.class);
    doAnswer(new Answer() {

      public Object answer(InvocationOnMock invocation) throws Throwable {
        System.out.println("Handle!");
        return null;
      }

    }).when(h).handleMessage(any(InputMessage.class), any(JSONObject.class));
    ms.consume("input1", h);
    // So: right here, we expect that the original
    ms.consume("input2", h);
    Assert.assertEquals(null, violation[0]);
  }
}
