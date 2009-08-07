/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import net.sf.json.JSONObject;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import static org.junit.Assert.*;

import static org.mockito.Mockito.*;
import static org.mockito.Matchers.*;

/**
 *
 * @author mikeb
 */
public class SerialisedInputConsumerTest {

  public SerialisedInputConsumerTest() {
    }

  Channel channel;
  QueueingConsumer consumer;
  InputHandler handler;
  Logger log;
  Delivery delivery;

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

    @Before
    public void setUp() {
      channel = mock(Channel.class);
      consumer = mock(QueueingConsumer.class);
      when(consumer.getChannel()).thenReturn(channel);
      handler = mock(InputHandler.class);
      log = mock(Logger.class);
      delivery = mock(Delivery.class);
      when(delivery.getProperties()).thenReturn(new BasicProperties());
      Envelope e = mock(Envelope.class);
      when(e.getDeliveryTag()).thenReturn(100L);
      when(delivery.getEnvelope()).thenReturn(e);

      // We have to be careful here.  The InputConsumer will exit if the channel closes, and block on
      // consumer.nextDelivery().  If we want the input consumer to run once then exit, we have a race between it getting past the channel
      // closed condition and us setting it closed.  To avoid that, we put a wrapper around nextDelivery that closes
      // the channel.
      // Exercise for the reader: what should change in the design so that this isn't necessary?
      final boolean[] sync = {true};
      when(channel.isOpen()).thenAnswer(new Answer() {
        public Boolean answer(InvocationOnMock invocation) throws Throwable {
          return sync[0];
        }
      });
      try {
        when(consumer.nextDelivery()).thenAnswer(new Answer<Delivery>() {
          public Delivery answer(InvocationOnMock invocation) throws Throwable {
            sync[0] = false;
            return delivery;
          }
        });
      }
      catch (InterruptedException ex) {
        // satisfy the type checker;
        // we're mocking, so it won't throw the exception
        // despite declaring it.
      }
    }

    @After
    public void tearDown() {
    }

  /**
   * Test of run method, of class SerialisedInputConsumer.
   */
  @Test
  public void testMessageGetsAcked() throws Exception {
    Object lock = new Object();
    SerialisedInputConsumer ic = new SerialisedInputConsumer(consumer, handler, new JSONObject(), log, lock);
    ic.run();
    verifyZeroInteractions(log);
    verify(channel).basicAck(anyLong(), anyBoolean());
    verify(channel).txCommit();
  }

  @Test
  public void testMessageRollbackNoAckOnException() throws Exception {
    Object lock = new Object();
    doThrow(new PluginException("Something bad happened")).when(handler).handleMessage(any(InputMessage.class), any(JSONObject.class));
    SerialisedInputConsumer ic = new SerialisedInputConsumer(consumer, handler, new JSONObject(), log, lock);
    ic.run();
    verify(channel).txRollback();
    verify(channel, never()).basicAck(anyLong(), anyBoolean());
  }
}
