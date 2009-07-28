package com.rabbitmq.streams.harness;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.Map;
import java.util.Collections;
import java.util.HashMap;

import com.rabbitmq.client.AlreadyClosedException;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.impl.ChannelN;

public class AMQPLogger implements Runnable, Logger {
  private static final Map<String, Object> noheaders = Collections.emptyMap();

  private static final String logExchange = "feedshub/log";
  private static final String headerName = "com.rabbitmq.streams.logging.label";

  private final LogMessage shutdownMessage = new LogMessage(LogLevel.Shutdown, null, noheaders);

  private final Channel logChannel;
  private final String logRoutingKey;

  private final BlockingQueue<LogMessage> logQueue = new SynchronousQueue<LogMessage>();

  public AMQPLogger(Channel logChannel, String logRoutingKey) {
    this.logChannel = logChannel;
    this.logRoutingKey = logRoutingKey;
  }

  private String throwableToString(Throwable t) {
    StringWriter out = new StringWriter();
    t.printStackTrace(new PrintWriter(out));
    return out.toString();
  }

  private Map<String, Object> labelToHeader(String label) {
    HashMap<String, Object> map = new HashMap<String, Object>();
    map.put(headerName, label);

    return map;
  }

  public void debug(Throwable t) {
    debug(throwableToString(t), noheaders);
  }

  public void debug(Throwable t, String label) {
    debug(throwableToString(t), labelToHeader(label));
  }

  public void info(Throwable t) {
    info(throwableToString(t));
  }

  public void warn(Throwable t) {
    warn(throwableToString(t), noheaders);
  }

  public void error(Throwable t) {
    error(throwableToString(t), noheaders);
  }

  public void fatal(Throwable t) {
    fatal(throwableToString(t), noheaders);
  }

  public void debug(String message) {
    debug(message, noheaders);
  }

  public void info(String message) {
    info(message, noheaders);
  }

  public void warn(String message)  {
    warn(message, noheaders);
  }

  public void error(String message) {
    error(message, noheaders);
  }

  public void fatal(String message) {
    fatal(message, noheaders);
  }

  public void debug(String message, String label) {
    debug(message,labelToHeader(label));
  }

  public void info(String message, String label) {
    info(message,labelToHeader(label));
  }

  public void warn(String message, String label)  {
    warn(message, labelToHeader(label));
  }

  public void error(String message, String label) {
    error(message, labelToHeader(label));
  }

  public void fatal(String message, String label) {
    fatal(message,labelToHeader(label));
  }

  public void debug(String message, Map<String, Object> headers) {
    try {
      logQueue.put(new LogMessage(LogLevel.Debug, message, headers));
    }
    catch (InterruptedException e) {
      debug(message, headers);
    }
  }

  public void info(String message, Map<String, Object> headers) {
    try {
      logQueue.put(new LogMessage(LogLevel.Info, message, headers));
    }
    catch (InterruptedException e) {
      info(message, headers);
    }
  }

  public void warn(String message, Map<String, Object> headers) {
    try {
      logQueue.put(new LogMessage(LogLevel.Warn, message, headers));
    }
    catch (InterruptedException e) {
      warn(message, headers);
    }
  }

  public void error(String message, Map<String, Object> headers) {
    try {
      logQueue.put(new LogMessage(LogLevel.Error, message,headers));
    }
    catch (InterruptedException e) {
      error(message, headers);
    }
  }

  public void fatal(String message, Map<String, Object> headers) {
    try {
      logQueue.put(new LogMessage(LogLevel.Fatal, message, headers));
    }
    catch (InterruptedException e) {
      fatal(message, headers);
    }
  }

  public void shutdown() {
    try {
      logQueue.put(shutdownMessage);
    }
    catch (InterruptedException e) {
      shutdown();
    }
  }

  public void run() {
    LogMessage msg;
    while (true) {
      msg = null;
      while (null == msg) {
        try {
          msg = logQueue.take();
        }
        catch (InterruptedException ignored) {
        }
      }
      if (msg == shutdownMessage) {
        break;
      }
      else {
        msg.log();
      }
    }
    if (logChannel.isOpen()) {
      try {
        logChannel.close();
      }
      catch (ShutdownSignalException ignored) {
      }
      catch (IOException ignored) {
      }
    }
  }

  private BasicProperties basicProperties() {
    BasicProperties basicProperties = new BasicProperties();
    basicProperties.deliveryMode = 2;

    return basicProperties;
  }

  private class LogMessage {
    private final LogLevel level;
    private final String message;
    private final Map<String, Object> headers;

    LogMessage(LogLevel level, String message, Map<String, Object> headers) {
      this.level = level;
      this.message = message;
      this.headers = headers;
    }

    void log() {
      BasicProperties basicProperties = basicProperties();
      basicProperties().headers = headers;
      level.log(logChannel, logRoutingKey, basicProperties, message);
    }
  }

  private static enum LogLevel {
    Info("info"), Warn("warn"), Error("error"), Fatal("fatal"), Shutdown("shutdown"), Debug("debug");

    private final String level;

    LogLevel(String level) {
      this.level = level;
    }

    void log(Channel logChannel, String routingKey, BasicProperties basicProperties, String message) {
      try {
        try {
          logChannel.basicPublish(logExchange, level + routingKey, false, false, basicProperties, message.getBytes());
        }
        catch (AlreadyClosedException ace) {
          System.err.println(level + routingKey + ": " + message);
          ace.printStackTrace();
        }
      }
      catch (IOException e) {
        System.err.println(level + routingKey + ": " + message);
        e.printStackTrace();
      }
    }
  }

}
